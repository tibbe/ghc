-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2006
--
-- GHC's lexer.
--
-- This is a combination of an Alex-generated lexer from a regex
-- definition, with some hand-coded bits.
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a start and end SrcLoc attached to it.
--
-----------------------------------------------------------------------------

--   ToDo / known bugs:
--    - parsing integers is a bit slow
--    - readRational is a bit slow
--
--   Known bugs, that were also in the previous version:
--    - M... should be 3 tokens, not 1.
--    - pragma-end should be only valid in a pragma

--   qualified operator NOTES.
--   
--   - If M.(+) is a single lexeme, then..
--     - Probably (+) should be a single lexeme too, for consistency.
--       Otherwise ( + ) would be a prefix operator, but M.( + ) would not be.
--     - But we have to rule out reserved operators, otherwise (..) becomes
--       a different lexeme.
--     - Should we therefore also rule out reserved operators in the qualified
--       form?  This is quite difficult to achieve.  We don't do it for
--       qualified varids.

{
{-# OPTIONS -Wwarn -w #-}
-- The above -Wwarn supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details
--
-- Note that Alex itself generates code with with some unused bindings and
-- without type signatures, so removing the flag might not be possible.

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer (
   Token(..), lexer, pragState, mkPState, PState(..),
   P(..), ParseResult(..), getSrcLoc, 
   getPState, getDynFlags, withThisPackage,
   failLocMsgP, failSpanMsgP, srcParseFail,
   getMessages, 
   popContext, pushCurrentContext, setLastToken, setSrcLoc,
   getLexState, popLexState, pushLexState,
   extension, standaloneDerivingEnabled, bangPatEnabled,
   addWarning,
   lexTokenStream
  ) where

import Bag
import ErrUtils
import Outputable
import StringBuffer
import FastString
import SrcLoc
import UniqFM
import DynFlags
import Module
import Ctype
import Util		( readRational )

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
}

$unispace    = \x05 -- Trick Alex into handling Unicode. See alexGetChar.
$whitechar   = [\ \n\r\f\v $unispace]
$white_no_nl = $whitechar # \n
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$unigraphic = \x06 -- Trick Alex into handling Unicode. See alexGetChar.
$graphic   = [$small $large $symbol $digit $special $unigraphic \:\"\']

$octit	   = 0-7
$hexit     = [$decdigit A-F a-f]
$symchar   = [$symbol \:]
$nl        = [\n\r]
$idchar    = [$small $large $digit \']

$pragmachar = [$small $large $digit]

$docsym    = [\| \^ \* \$]

@varid     = $small $idchar*
@conid     = $large $idchar*

@varsym    = $symbol $symchar*
@consym    = \: $symchar*

@decimal     = $decdigit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

-- we support the hierarchical module name extension:
@qual = (@conid \.)+

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-
@signed = @negative ?

haskell :-

-- everywhere: skip whitespace and comments
$white_no_nl+ 				;
$tab+         { warn Opt_WarnTabs (text "Tab character") }

-- Everywhere: deal with nested comments.  We explicitly rule out
-- pragmas, "{-#", so that we don't accidentally treat them as comments.
-- (this can happen even though pragmas will normally take precedence due to
-- longest-match, because pragmas aren't valid in every state, but comments
-- are). We also rule out nested Haddock comments, if the -haddock flag is
-- set.

"{-" / { isNormalComment } { nested_comment lexToken }

-- Single-line comments are a bit tricky.  Haskell 98 says that two or
-- more dashes followed by a symbol should be parsed as a varsym, so we
-- have to exclude those.

-- Since Haddock comments aren't valid in every state, we need to rule them
-- out here.  

-- The following two rules match comments that begin with two dashes, but
-- continue with a different character. The rules test that this character
-- is not a symbol (in which case we'd have a varsym), and that it's not a
-- space followed by a Haddock comment symbol (docsym) (in which case we'd
-- have a Haddock comment). The rules then munch the rest of the line.

"-- " ~[$docsym \#] .* { lineCommentToken }
"--" [^$symbol : \ ] .* { lineCommentToken }

-- Next, match Haddock comments if no -haddock flag

"-- " [$docsym \#] .* / { ifExtension (not . haddockEnabled) } { lineCommentToken }

-- Now, when we've matched comments that begin with 2 dashes and continue
-- with a different character, we need to match comments that begin with three
-- or more dashes (which clearly can't be Haddock comments). We only need to
-- make sure that the first non-dash character isn't a symbol, and munch the
-- rest of the line.

"---"\-* [^$symbol :] .* { lineCommentToken }

-- Since the previous rules all match dashes followed by at least one
-- character, we also need to match a whole line filled with just dashes.

"--"\-* / { atEOL } { lineCommentToken }

-- We need this rule since none of the other single line comment rules
-- actually match this case.

"-- " / { atEOL } { lineCommentToken }

-- 'bol' state: beginning of a line.  Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
--
-- One slight wibble here: what if the line begins with {-#? In
-- theory, we have to lex the pragma to see if it's one we recognise,
-- and if it is, then we backtrack and do_bol, otherwise we treat it
-- as a nested comment.  We don't bother with this: if the line begins
-- with {-#, then we'll assume it's a pragma we know about and go for do_bol.
<bol> {
  \n					;
  ^\# (line)?				{ begin line_prag1 }
  ^\# pragma .* \n			; -- GCC 3.3 CPP generated, apparently
  ^\# \! .* \n				; -- #!, for scripts
  ()					{ do_bol }
}

-- after a layout keyword (let, where, do, of), we begin a new layout
-- context if the curly brace is missing.
-- Careful! This stuff is quite delicate.
<layout, layout_do> {
  \{ / { notFollowedBy '-' }		{ pop_and open_brace }
	-- we might encounter {-# here, but {- has been handled already
  \n					;
  ^\# (line)?				{ begin line_prag1 }
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()				{ new_layout_context True }
<layout_do> ()				{ new_layout_context False }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()			{ do_layout_left }

<0,option_prags> \n				{ begin bol }

"{-#" $whitechar* $pragmachar+ / { known_pragma linePrags }
                                { dispatch_pragmas linePrags }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag1> $decdigit+			{ setLine line_prag1a }
<line_prag1a> \" [$graphic \ ]* \"	{ setFile line_prag1b }
<line_prag1b> .*			{ pop }

-- Haskell-style line pragmas, of the form
--    {-# LINE <line> "<file>" #-}
<line_prag2> $decdigit+			{ setLine line_prag2a }
<line_prag2a> \" [$graphic \ ]* \"	{ setFile line_prag2b }
<line_prag2b> "#-}"|"-}"		{ pop }
   -- NOTE: accept -} at the end of a LINE pragma, for compatibility
   -- with older versions of GHC which generated these.

<0,option_prags> {
  "{-#" $whitechar* $pragmachar+ 
        $whitechar+ $pragmachar+ / { known_pragma twoWordPrags }
                                 { dispatch_pragmas twoWordPrags }

  "{-#" $whitechar* $pragmachar+ / { known_pragma oneWordPrags }
                                 { dispatch_pragmas oneWordPrags }

  -- We ignore all these pragmas, but don't generate a warning for them
  "{-#" $whitechar* $pragmachar+ / { known_pragma ignoredPrags }
                                 { dispatch_pragmas ignoredPrags }

  -- ToDo: should only be valid inside a pragma:
  "#-}" 				{ endPrag }
}

<option_prags> {
  "{-#"  $whitechar* $pragmachar+ / { known_pragma fileHeaderPrags }
                                   { dispatch_pragmas fileHeaderPrags }

  "-- #"                                 { multiline_doc_comment }
}

<0> {
  -- In the "0" mode we ignore these pragmas
  "{-#"  $whitechar* $pragmachar+ / { known_pragma fileHeaderPrags }
                     { nested_comment lexToken }
}

<0> {
  "-- #" .* { lineCommentToken }
}

<0,option_prags> {
  "{-#"  { warnThen Opt_WarnUnrecognisedPragmas (text "Unrecognised pragma")
                    (nested_comment lexToken) }
}

-- '0' state: ordinary lexemes

-- Haddock comments

<0> {
  "-- " $docsym      / { ifExtension haddockEnabled } { multiline_doc_comment }
  "{-" \ ? $docsym   / { ifExtension haddockEnabled } { nested_doc_comment }
}

-- "special" symbols

<0> {
  "[:" / { ifExtension parrEnabled }	{ token ITopabrack }
  ":]" / { ifExtension parrEnabled }	{ token ITcpabrack }
}
  
<0> {
  "[|"	    / { ifExtension thEnabled }	{ token ITopenExpQuote }
  "[e|"	    / { ifExtension thEnabled }	{ token ITopenExpQuote }
  "[p|"	    / { ifExtension thEnabled }	{ token ITopenPatQuote }
  "[d|"	    / { ifExtension thEnabled }	{ layout_token ITopenDecQuote }
  "[t|"	    / { ifExtension thEnabled }	{ token ITopenTypQuote }
  "|]"	    / { ifExtension thEnabled }	{ token ITcloseQuote }
  \$ @varid / { ifExtension thEnabled }	{ skip_one_varid ITidEscape }
  "$("	    / { ifExtension thEnabled }	{ token ITparenEscape }

  "[$" @varid "|"  / { ifExtension qqEnabled }
                     { lex_quasiquote_tok }
}

<0> {
  "(|" / { ifExtension arrowsEnabled `alexAndPred` notFollowedBySymbol }
					{ special IToparenbar }
  "|)" / { ifExtension arrowsEnabled }  { special ITcparenbar }
}

<0> {
  \? @varid / { ifExtension ipEnabled }	{ skip_one_varid ITdupipvarid }
}

<0> {
  "(#" / { ifExtension unboxedTuplesEnabled `alexAndPred` notFollowedBySymbol }
         { token IToubxparen }
  "#)" / { ifExtension unboxedTuplesEnabled }
         { token ITcubxparen }
}

<0> {
  "{|" / { ifExtension genericsEnabled } { token ITocurlybar }
  "|}" / { ifExtension genericsEnabled } { token ITccurlybar }
}

<0,option_prags> {
  \(					{ special IToparen }
  \)					{ special ITcparen }
  \[					{ special ITobrack }
  \]					{ special ITcbrack }
  \,					{ special ITcomma }
  \;					{ special ITsemi }
  \`					{ special ITbackquote }
 				
  \{					{ open_brace }
  \}					{ close_brace }
}

<0,option_prags> {
  @qual @varid			{ idtoken qvarid }
  @qual @conid			{ idtoken qconid }
  @varid			{ varid }
  @conid			{ idtoken conid }
}

<0> {
  @qual @varid "#"+ / { ifExtension magicHashEnabled } { idtoken qvarid }
  @qual @conid "#"+ / { ifExtension magicHashEnabled } { idtoken qconid }
  @varid "#"+       / { ifExtension magicHashEnabled } { varid }
  @conid "#"+       / { ifExtension magicHashEnabled } { idtoken conid }
}

-- ToDo: - move `var` and (sym) into lexical syntax?
--       - remove backquote from $special?
<0> {
  @qual @varsym       / { ifExtension oldQualOps } { idtoken qvarsym }
  @qual @consym       / { ifExtension oldQualOps } { idtoken qconsym }
  @qual \( @varsym \) / { ifExtension newQualOps } { idtoken prefixqvarsym }
  @qual \( @consym \) / { ifExtension newQualOps } { idtoken prefixqconsym }
  @varsym                                          { varsym }
  @consym                                          { consym }
}

-- For the normal boxed literals we need to be careful
-- when trying to be close to Haskell98
<0> {
  -- Normal integral literals (:: Num a => a, from Integer)
  @decimal           { tok_num positive 0 0 decimal }
  0[oO] @octal       { tok_num positive 2 2 octal }
  0[xX] @hexadecimal { tok_num positive 2 2 hexadecimal }

  -- Normal rational literals (:: Fractional a => a, from Rational)
  @floating_point    { strtoken tok_float }
}

<0> {
  -- Unboxed ints (:: Int#) and words (:: Word#)
  -- It's simpler (and faster?) to give separate cases to the negatives,
  -- especially considering octal/hexadecimal prefixes.
  @decimal                     \# / { ifExtension magicHashEnabled } { tok_primint positive 0 1 decimal }
  0[oO] @octal                 \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 octal }
  0[xX] @hexadecimal           \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 hexadecimal }
  @negative @decimal           \# / { ifExtension magicHashEnabled } { tok_primint negative 1 2 decimal }
  @negative 0[oO] @octal       \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 octal }
  @negative 0[xX] @hexadecimal \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 hexadecimal }

  @decimal                     \# \# / { ifExtension magicHashEnabled } { tok_primword 0 2 decimal }
  0[oO] @octal                 \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 octal }
  0[xX] @hexadecimal           \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 hexadecimal }

  -- Unboxed floats and doubles (:: Float#, :: Double#)
  -- prim_{float,double} work with signed literals
  @signed @floating_point \# / { ifExtension magicHashEnabled } { init_strtoken 1 tok_primfloat }
  @signed @floating_point \# \# / { ifExtension magicHashEnabled } { init_strtoken 2 tok_primdouble }
}

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'				{ lex_char_tok }
  \" 				{ lex_string_tok }
}

{
-- -----------------------------------------------------------------------------
-- The token type

data Token
  = ITas  			-- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo
  | ITelse
  | IThiding
  | ITif
  | ITimport
  | ITin
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITinstance
  | ITlet
  | ITmodule
  | ITnewtype
  | ITof
  | ITqualified
  | ITthen
  | ITtype
  | ITwhere
  | ITscc			-- ToDo: remove (we use {-# SCC "..." #-} now)

  | ITforall			-- GHC extension keywords
  | ITforeign
  | ITexport
  | ITlabel
  | ITdynamic
  | ITsafe
  | ITthreadsafe
  | ITunsafe
  | ITstdcallconv
  | ITccallconv
  | ITprimcallconv
  | ITmdo
  | ITfamily
  | ITgroup
  | ITby
  | ITusing

	-- Pragmas
  | ITinline_prag Bool		-- True <=> INLINE, False <=> NOINLINE
  | ITinline_conlike_prag Bool  -- same
  | ITspec_prag			-- SPECIALISE	
  | ITspec_inline_prag Bool	-- SPECIALISE INLINE (or NOINLINE)
  | ITsource_prag
  | ITrules_prag
  | ITwarning_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITscc_prag
  | ITgenerated_prag
  | ITcore_prag                 -- hdaume: core annotations
  | ITunpack_prag
  | ITann_prag
  | ITclose_prag
  | IToptions_prag String
  | ITinclude_prag String
  | ITlanguage_prag

  | ITdotdot  			-- reserved symbols
  | ITcolon
  | ITdcolon
  | ITequal
  | ITlam
  | ITvbar
  | ITlarrow
  | ITrarrow
  | ITat
  | ITtilde
  | ITdarrow
  | ITminus
  | ITbang
  | ITstar
  | ITdot

  | ITbiglam			-- GHC-extension symbols

  | ITocurly  			-- special symbols
  | ITccurly
  | ITocurlybar                 -- {|, for type applications
  | ITccurlybar                 -- |}, for type applications
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITopabrack			-- [:, for parallel arrays with -XParr
  | ITcpabrack			-- :], for parallel arrays with -XParr
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote

  | ITvarid   FastString	-- identifiers
  | ITconid   FastString
  | ITvarsym  FastString
  | ITconsym  FastString
  | ITqvarid  (FastString,FastString)
  | ITqconid  (FastString,FastString)
  | ITqvarsym (FastString,FastString)
  | ITqconsym (FastString,FastString)
  | ITprefixqvarsym (FastString,FastString)
  | ITprefixqconsym (FastString,FastString)

  | ITdupipvarid   FastString	-- GHC extension: implicit param: ?x

  | ITchar       Char
  | ITstring     FastString
  | ITinteger    Integer
  | ITrational   Rational

  | ITprimchar   Char
  | ITprimstring FastString
  | ITprimint    Integer
  | ITprimword   Integer
  | ITprimfloat  Rational
  | ITprimdouble Rational

  -- Template Haskell extension tokens
  | ITopenExpQuote  		--  [| or [e|
  | ITopenPatQuote		--  [p|
  | ITopenDecQuote		--  [d|
  | ITopenTypQuote		--  [t|         
  | ITcloseQuote		--  |]
  | ITidEscape   FastString	--  $x
  | ITparenEscape		--  $( 
  | ITvarQuote			--  '
  | ITtyQuote			--  ''
  | ITquasiQuote (FastString,FastString,SrcSpan) --  [:...|...|]

  -- Arrow notation extension
  | ITproc
  | ITrec
  | IToparenbar			--  (|
  | ITcparenbar			--  |)
  | ITlarrowtail		--  -<
  | ITrarrowtail		--  >-
  | ITLarrowtail		--  -<<
  | ITRarrowtail		--  >>-

  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof			-- end of file token

  -- Documentation annotations
  | ITdocCommentNext  String     -- something beginning '-- |'
  | ITdocCommentPrev  String     -- something beginning '-- ^'
  | ITdocCommentNamed String     -- something beginning '-- $'
  | ITdocSection      Int String -- a section heading
  | ITdocOptions      String     -- doc options (prune, ignore-exports, etc)
  | ITdocOptionsOld   String     -- doc options declared "-- # ..."-style
  | ITlineComment     String     -- comment starting by "--"
  | ITblockComment    String     -- comment in {- -}

#ifdef DEBUG
  deriving Show -- debugging
#endif

{-
isSpecial :: Token -> Bool
-- If we see M.x, where x is a keyword, but
-- is special, we treat is as just plain M.x, 
-- not as a keyword.
isSpecial ITas        	= True
isSpecial IThiding    	= True
isSpecial ITqualified 	= True
isSpecial ITforall    	= True
isSpecial ITexport    	= True
isSpecial ITlabel     	= True
isSpecial ITdynamic   	= True
isSpecial ITsafe    	= True
isSpecial ITthreadsafe 	= True
isSpecial ITunsafe    	= True
isSpecial ITccallconv   = True
isSpecial ITstdcallconv = True
isSpecial ITprimcallconv = True
isSpecial ITmdo		= True
isSpecial ITfamily	= True
isSpecial ITgroup   = True
isSpecial ITby      = True
isSpecial ITusing   = True
isSpecial _             = False
-}

-- the bitmap provided as the third component indicates whether the
-- corresponding extension keyword is valid under the extension options
-- provided to the compiler; if the extension corresponding to *any* of the
-- bits set in the bitmap is enabled, the keyword is valid (this setup
-- facilitates using a keyword in two different extensions that can be
-- activated independently)
--
reservedWordsFM :: UniqFM (Token, Int)
reservedWordsFM = listToUFM $
	map (\(x, y, z) -> (mkFastString x, (y, z)))
       [( "_",		ITunderscore, 	0 ),
	( "as",		ITas, 		0 ),
	( "case",	ITcase, 	0 ),     
	( "class",	ITclass, 	0 ),    
	( "data",	ITdata, 	0 ),     
	( "default",	ITdefault, 	0 ),  
	( "deriving",	ITderiving, 	0 ), 
	( "do",		ITdo, 		0 ),       
	( "else",	ITelse, 	0 ),     
	( "hiding",	IThiding, 	0 ),
	( "if",		ITif, 		0 ),       
	( "import",	ITimport, 	0 ),   
	( "in",		ITin, 		0 ),       
	( "infix",	ITinfix, 	0 ),    
	( "infixl",	ITinfixl, 	0 ),   
	( "infixr",	ITinfixr, 	0 ),   
	( "instance",	ITinstance, 	0 ), 
	( "let",	ITlet, 		0 ),      
	( "module",	ITmodule, 	0 ),   
	( "newtype",	ITnewtype, 	0 ),  
	( "of",		ITof, 		0 ),       
	( "qualified",	ITqualified, 	0 ),
	( "then",	ITthen, 	0 ),     
	( "type",	ITtype, 	0 ),     
	( "where",	ITwhere, 	0 ),
	( "_scc_",	ITscc, 		0 ),		-- ToDo: remove

    ( "forall",	ITforall,	 bit explicitForallBit .|. bit inRulePragBit),
	( "mdo",	ITmdo,		 bit recursiveDoBit),
	( "family",	ITfamily,	 bit tyFamBit),
    ( "group",  ITgroup,     bit transformComprehensionsBit),
    ( "by",     ITby,        bit transformComprehensionsBit),
    ( "using",  ITusing,     bit transformComprehensionsBit),

	( "foreign",	ITforeign,	 bit ffiBit),
	( "export",	ITexport,	 bit ffiBit),
	( "label",	ITlabel,	 bit ffiBit),
	( "dynamic",	ITdynamic,	 bit ffiBit),
	( "safe",	ITsafe,		 bit ffiBit),
	( "threadsafe",	ITthreadsafe,	 bit ffiBit),  -- ToDo: remove
	( "unsafe",	ITunsafe,	 bit ffiBit),
	( "stdcall",    ITstdcallconv,	 bit ffiBit),
	( "ccall",      ITccallconv,	 bit ffiBit),
	( "prim",       ITprimcallconv,	 bit ffiBit),

	( "rec",	ITrec,		 bit recBit),
	( "proc",	ITproc,		 bit arrowsBit)
     ]

reservedSymsFM :: UniqFM (Token, Int -> Bool)
reservedSymsFM = listToUFM $
    map (\ (x,y,z) -> (mkFastString x,(y,z)))
      [ ("..",  ITdotdot,   always)
        -- (:) is a reserved op, meaning only list cons
       ,(":",   ITcolon,    always)
       ,("::",  ITdcolon,   always)
       ,("=",   ITequal,    always)
       ,("\\",  ITlam,      always)
       ,("|",   ITvbar,     always)
       ,("<-",  ITlarrow,   always)
       ,("->",  ITrarrow,   always)
       ,("@",   ITat,       always)
       ,("~",   ITtilde,    always)
       ,("=>",  ITdarrow,   always)
       ,("-",   ITminus,    always)
       ,("!",   ITbang,     always)

        -- For data T (a::*) = MkT
       ,("*", ITstar, always) -- \i -> kindSigsEnabled i || tyFamEnabled i)
        -- For 'forall a . t'
       ,(".", ITdot,  always) -- \i -> explicitForallEnabled i || inRulePrag i)

       ,("-<",  ITlarrowtail, arrowsEnabled)
       ,(">-",  ITrarrowtail, arrowsEnabled)
       ,("-<<", ITLarrowtail, arrowsEnabled)
       ,(">>-", ITRarrowtail, arrowsEnabled)

       ,("∷",   ITdcolon, unicodeSyntaxEnabled)
       ,("⇒",   ITdarrow, unicodeSyntaxEnabled)
       ,("∀",   ITforall, \i -> unicodeSyntaxEnabled i &&
                                explicitForallEnabled i)
       ,("→",   ITrarrow, unicodeSyntaxEnabled)
       ,("←",   ITlarrow, unicodeSyntaxEnabled)
       ,("⋯",   ITdotdot, unicodeSyntaxEnabled)
        -- ToDo: ideally, → and ∷ should be "specials", so that they cannot
        -- form part of a large operator.  This would let us have a better
        -- syntax for kinds: ɑ∷*→* would be a legal kind signature. (maybe).
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = SrcSpan -> StringBuffer -> Int -> P (Located Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

token, layout_token :: Token -> Action
token t span _buf _len = return (L span t)
layout_token t span _buf _len = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

skip_one_varid :: (FastString -> Token) -> Action
skip_one_varid f span buf len 
  = return (L span $! f (lexemeToFastString (stepOn buf) (len-1)))

strtoken :: (String -> Token) -> Action
strtoken f span buf len = 
  return (L span $! (f $! lexemeToString buf len))

init_strtoken :: Int -> (String -> Token) -> Action
-- like strtoken, but drops the last N character(s)
init_strtoken drop f span buf len = 
  return (L span $! (f $! lexemeToString buf (len-drop)))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do popLexState; lexToken

pop_and :: Action -> Action
pop_and act span buf len = do popLexState; act span buf len

{-# INLINE nextCharIs #-}
nextCharIs :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIs buf p = not (atEnd buf) && p (currentChar buf)

notFollowedBy :: Char -> AlexAccPred Int
notFollowedBy char _ _ _ (AI _ _ buf) 
  = nextCharIs buf (/=char)

notFollowedBySymbol :: AlexAccPred Int
notFollowedBySymbol _ _ _ (AI _ _ buf)
  = nextCharIs buf (`notElem` "!#$%&*+./<=>?@\\^|-~")

-- We must reject doc comments as being ordinary comments everywhere.
-- In some cases the doc comment will be selected as the lexeme due to
-- maximal munch, but not always, because the nested comment rule is
-- valid in all states, but the doc-comment rules are only valid in
-- the non-layout states.
isNormalComment :: AlexAccPred Int
isNormalComment bits _ _ (AI _ _ buf)
  | haddockEnabled bits = notFollowedByDocOrPragma
  | otherwise           = nextCharIs buf (/='#')
  where
    notFollowedByDocOrPragma
       = not $ spaceAndP buf (`nextCharIs` (`elem` "|^*$#"))

spaceAndP :: StringBuffer -> (StringBuffer -> Bool) -> Bool
spaceAndP buf p = p buf || nextCharIs buf (==' ') && p (snd (nextChar buf))

{-
haddockDisabledAnd p bits _ _ (AI _ _ buf)
  = if haddockEnabled bits then False else (p buf)
-}

atEOL :: AlexAccPred Int
atEOL _ _ _ (AI _ _ buf) = atEnd buf || currentChar buf == '\n'

ifExtension :: (Int -> Bool) -> AlexAccPred Int
ifExtension pred bits _ _ _ = pred bits

multiline_doc_comment :: Action
multiline_doc_comment span buf _len = withLexedDocType (worker "")
  where
    worker commentAcc input docType oneLine = case alexGetChar input of
      Just ('\n', input') 
        | oneLine -> docCommentEnd input commentAcc docType buf span
        | otherwise -> case checkIfCommentLine input' of
          Just input -> worker ('\n':commentAcc) input docType False
          Nothing -> docCommentEnd input commentAcc docType buf span
      Just (c, input) -> worker (c:commentAcc) input docType oneLine
      Nothing -> docCommentEnd input commentAcc docType buf span
      
    checkIfCommentLine input = check (dropNonNewlineSpace input)
      where
        check input = case alexGetChar input of
          Just ('-', input) -> case alexGetChar input of
            Just ('-', input) -> case alexGetChar input of
              Just (c, _) | c /= '-' -> Just input
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing

        dropNonNewlineSpace input = case alexGetChar input of
          Just (c, input') 
            | isSpace c && c /= '\n' -> dropNonNewlineSpace input'
            | otherwise -> input
          Nothing -> input

lineCommentToken :: Action
lineCommentToken span buf len = do
  b <- extension rawTokenStreamEnabled
  if b then strtoken ITlineComment span buf len else lexToken

{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: P (Located Token) -> Action
nested_comment cont span _str _len = do
  input <- getInput
  go "" (1::Int) input
  where
    go commentAcc 0 input = do setInput input
                               b <- extension rawTokenStreamEnabled
                               if b
                                 then docCommentEnd input commentAcc ITblockComment _str span
                                 else cont
    go commentAcc n input = case alexGetChar input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go commentAcc (n-1) input
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar input of
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

nested_doc_comment :: Action
nested_doc_comment span buf _len = withLexedDocType (go "")
  where
    go commentAcc input docType _ = case alexGetChar input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar input of
        Nothing -> errBrace input span
        Just ('\125',input) ->
          docCommentEnd input commentAcc docType buf span
        Just (_,_) -> go ('-':commentAcc) input docType False
      Just ('\123', input) -> case alexGetChar input of
        Nothing  -> errBrace input span
        Just ('-',input) -> do
          setInput input
          let cont = do input <- getInput; go commentAcc input docType False
          nested_comment cont span buf _len
        Just (_,_) -> go ('\123':commentAcc) input docType False
      Just (c,input) -> go (c:commentAcc) input docType False

withLexedDocType :: (AlexInput -> (String -> Token) -> Bool -> P (Located Token))
                 -> P (Located Token)
withLexedDocType lexDocComment = do
  input@(AI _ _ buf) <- getInput
  case prevChar buf ' ' of
    '|' -> lexDocComment input ITdocCommentNext False
    '^' -> lexDocComment input ITdocCommentPrev False
    '$' -> lexDocComment input ITdocCommentNamed False
    '*' -> lexDocSection 1 input
    '#' -> lexDocComment input ITdocOptionsOld False
    _ -> panic "withLexedDocType: Bad doc type"
 where 
    lexDocSection n input = case alexGetChar input of 
      Just ('*', input) -> lexDocSection (n+1) input
      Just (_,   _)     -> lexDocComment input (ITdocSection n) True
      Nothing -> do setInput input; lexToken -- eof reached, lex it normally

-- RULES pragmas turn on the forall and '.' keywords, and we turn them
-- off again at the end of the pragma.
rulePrag :: Action
rulePrag span _buf _len = do
  setExts (.|. bit inRulePragBit)
  return (L span ITrules_prag)

endPrag :: Action
endPrag span _buf _len = do
  setExts (.&. complement (bit inRulePragBit))
  return (L span ITclose_prag)

-- docCommentEnd
-------------------------------------------------------------------------------
-- This function is quite tricky. We can't just return a new token, we also
-- need to update the state of the parser. Why? Because the token is longer
-- than what was lexed by Alex, and the lexToken function doesn't know this, so 
-- it writes the wrong token length to the parser state. This function is
-- called afterwards, so it can just update the state. 

-- This is complicated by the fact that Haddock tokens can span multiple lines, 
-- which is something that the original lexer didn't account for. 
-- I have added last_line_len in the parser state which represents the length 
-- of the part of the token that is on the last line. It is now used for layout 
-- calculation in pushCurrentContext instead of last_len. last_len is, like it 
-- was before, the full length of the token, and it is now only used for error
-- messages. /Waern 

docCommentEnd :: AlexInput -> String -> (String -> Token) -> StringBuffer ->
                 SrcSpan -> P (Located Token) 
docCommentEnd input commentAcc docType buf span = do
  setInput input
  let (AI loc last_offs nextBuf) = input
      comment = reverse commentAcc
      span' = mkSrcSpan (srcSpanStart span) loc
      last_len = byteDiff buf nextBuf
      
      last_line_len = if (last_offs - last_len < 0) 
        then last_offs
        else last_len  
  
  span `seq` setLastToken span' last_len last_line_len
  return (L span' (docType comment))
 
errBrace :: AlexInput -> SrcSpan -> P a
errBrace (AI end _ _) span = failLocMsgP (srcSpanStart span) end "unterminated `{-'"

open_brace, close_brace :: Action
open_brace span _str _len = do 
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len = do 
  popContext
  return (L span ITccurly)

qvarid, qconid :: StringBuffer -> Int -> Token
qvarid buf len = ITqvarid $! splitQualName buf len False
qconid buf len = ITqconid $! splitQualName buf len False

splitQualName :: StringBuffer -> Int -> Bool -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
splitQualName orig_buf len parens = split orig_buf orig_buf
  where
    split buf dot_buf
	| orig_buf `byteDiff` buf >= len  = done dot_buf
	| c == '.'                	  = found_dot buf'
	| otherwise               	  = split buf' dot_buf
      where
       (c,buf') = nextChar buf
  
    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    found_dot buf -- buf points after the '.'
	| isUpper c    = split buf' buf
	| otherwise    = done buf
      where
       (c,buf') = nextChar buf

    done dot_buf =
	(lexemeToFastString orig_buf (qual_size - 1),
	 if parens -- Prelude.(+)
            then lexemeToFastString (stepOn dot_buf) (len - qual_size - 2)
            else lexemeToFastString dot_buf (len - qual_size))
      where
	qual_size = orig_buf `byteDiff` dot_buf

varid :: Action
varid span buf len =
  fs `seq`
  case lookupUFM reservedWordsFM fs of
	Just (keyword,0)    -> do
		maybe_layout keyword
		return (L span keyword)
	Just (keyword,exts) -> do
		b <- extension (\i -> exts .&. i /= 0)
		if b then do maybe_layout keyword
			     return (L span keyword)
		     else return (L span (ITvarid fs))
	_other -> return (L span (ITvarid fs))
  where
	fs = lexemeToFastString buf len

conid :: StringBuffer -> Int -> Token
conid buf len = ITconid fs
  where fs = lexemeToFastString buf len

qvarsym, qconsym, prefixqvarsym, prefixqconsym :: StringBuffer -> Int -> Token
qvarsym buf len = ITqvarsym $! splitQualName buf len False
qconsym buf len = ITqconsym $! splitQualName buf len False
prefixqvarsym buf len = ITprefixqvarsym $! splitQualName buf len True
prefixqconsym buf len = ITprefixqconsym $! splitQualName buf len True

varsym, consym :: Action
varsym = sym ITvarsym
consym = sym ITconsym

sym :: (FastString -> Token) -> SrcSpan -> StringBuffer -> Int
    -> P (Located Token)
sym con span buf len = 
  case lookupUFM reservedSymsFM fs of
	Just (keyword,exts) -> do
		b <- extension exts
		if b then return (L span keyword)
		     else return (L span $! con fs)
	_other -> return (L span $! con fs)
  where
	fs = lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (Integer -> Token)
     -> (Integer -> Integer)
 --    -> (StringBuffer -> StringBuffer) -> (Int -> Int)
     -> Int -> Int
     -> (Integer, (Char->Int)) -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len =
  return $ L span $ itint $! transint $ parseUnsignedInteger
     (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

-- some conveniences for use with tok_integral
tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral ITinteger
tok_primint :: (Integer -> Integer)
            -> Int -> Int
            -> (Integer, (Char->Int)) -> Action
tok_primint = tok_integral ITprimint
tok_primword :: Int -> Int
             -> (Integer, (Char->Int)) -> Action
tok_primword = tok_integral ITprimword positive
positive, negative :: (Integer -> Integer)
positive = id
negative = negate
decimal, octal, hexadecimal :: (Integer, Char -> Int)
decimal = (10,octDecDigit)
octal = (8,octDecDigit)
hexadecimal = (16,hexDigit)

-- readRational can understand negative rationals, exponents, everything.
tok_float, tok_primfloat, tok_primdouble :: String -> Token
tok_float        str = ITrational   $! readRational str
tok_primfloat    str = ITprimfloat  $! readRational str
tok_primdouble   str = ITprimdouble $! readRational str

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
	pos <- getOffside
	case pos of
	    LT -> do
                --trace "layout: inserting '}'" $ do
		popContext
		-- do NOT pop the lex state, we might have a ';' to insert
		return (L span ITvccurly)
	    EQ -> do
                --trace "layout: inserting ';'" $ do
		popLexState
		return (L span ITsemi)
	    GT -> do
		popLexState
		lexToken

-- certain keywords put us in the "layout" state, where we might
-- add an opening curly brace.
maybe_layout :: Token -> P ()
maybe_layout ITdo	= pushLexState layout_do
maybe_layout ITmdo	= pushLexState layout_do
maybe_layout ITof	= pushLexState layout
maybe_layout ITlet	= pushLexState layout
maybe_layout ITwhere	= pushLexState layout
maybe_layout ITrec	= pushLexState layout
maybe_layout _	        = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
--
new_layout_context :: Bool -> Action
new_layout_context strict span _buf _len = do
    popLexState
    (AI _ offset _) <- getInput
    ctx <- getContext
    case ctx of
	Layout prev_off : _  | 
	   (strict     && prev_off >= offset  ||
	    not strict && prev_off > offset) -> do
		-- token is indented to the left of the previous context.
		-- we must generate a {} sequence now.
		pushLexState layout_left
		return (L span ITvocurly)
	_ -> do
		setContext (Layout offset : ctx)
		return (L span ITvocurly)

do_layout_left :: Action
do_layout_left span _buf _len = do
    popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

setLine :: Int -> Action
setLine code span buf len = do
  let line = parseUnsignedInteger buf len 10 octDecDigit
  setSrcLoc (mkSrcLoc (srcSpanFile span) (fromIntegral line - 1) 0)
	-- subtract one: the line number refers to the *following* line
  popLexState
  pushLexState code
  lexToken

setFile :: Int -> Action
setFile code span buf len = do
  let file = lexemeToFastString (stepOn buf) (len-2)
  setSrcLoc (mkSrcLoc file (srcSpanEndLine span) (srcSpanEndCol span))
  popLexState
  pushLexState code
  lexToken


-- -----------------------------------------------------------------------------
-- Options, includes and language pragmas.

lex_string_prag :: (String -> Token) -> Action
lex_string_prag mkTok span _buf _len
    = do input <- getInput
         start <- getSrcLoc
         tok <- go [] input
         end <- getSrcLoc
         return (L (mkSrcSpan start end) tok)
    where go acc input
              = if isString input "#-}"
                   then do setInput input
                           return (mkTok (reverse acc))
                   else case alexGetChar input of
                          Just (c,i) -> go (c:acc) i
                          Nothing -> err input
          isString _ [] = True
          isString i (x:xs)
              = case alexGetChar i of
                  Just (c,i') | c == x    -> isString i' xs
                  _other -> False
          err (AI end _ _) = failLocMsgP (srcSpanStart span) end "unterminated options pragma"


-- -----------------------------------------------------------------------------
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok span _buf _len = do
  tok <- lex_string ""
  end <- getSrcLoc 
  return (L (mkSrcSpan (srcSpanStart span) end) tok)

lex_string :: String -> P Token
lex_string s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error

    Just ('"',i)  -> do
	setInput i
	magicHash <- extension magicHashEnabled
	if magicHash
	  then do
	    i <- getInput
	    case alexGetChar' i of
	      Just ('#',i) -> do
		   setInput i
		   if any (> '\xFF') s
                    then failMsgP "primitive string literal must contain only characters <= \'\\xFF\'"
                    else let s' = mkZFastString (reverse s) in
			 return (ITprimstring s')
			-- mkZFastString is a hack to avoid encoding the
			-- string in UTF-8.  We just want the exact bytes.
	      _other ->
		return (ITstring (mkFastString (reverse s)))
	  else
		return (ITstring (mkFastString (reverse s)))

    Just ('\\',i)
	| Just ('&',i) <- next -> do 
		setInput i; lex_string s
	| Just (c,i) <- next, is_space c -> do 
		setInput i; lex_stringgap s
	where next = alexGetChar' i

    Just (c, i) -> do
	c' <- lex_char c i
	lex_string (c':s)

lex_stringgap :: String -> P Token
lex_stringgap s = do
  c <- getCharOrFail
  case c of
    '\\' -> lex_string s
    c | is_space c -> lex_stringgap s
    _other -> lit_error


lex_char_tok :: Action
-- Here we are basically parsing character literals, such as 'x' or '\n'
-- but, when Template Haskell is on, we additionally spot
-- 'x and ''T, returning ITvarQuote and ITtyQuote respectively, 
-- but WITHOUT CONSUMING the x or T part  (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span _buf _len = do	-- We've seen '
   i1 <- getInput	-- Look ahead to first character
   let loc = srcSpanStart span
   case alexGetChar' i1 of
	Nothing -> lit_error 

	Just ('\'', i2@(AI end2 _ _)) -> do 	-- We've seen ''
		  th_exts <- extension thEnabled
		  if th_exts then do
			setInput i2
			return (L (mkSrcSpan loc end2)  ITtyQuote)
		   else lit_error

	Just ('\\', i2@(AI _end2 _ _)) -> do 	-- We've seen 'backslash
		  setInput i2
		  lit_ch <- lex_escape
		  mc <- getCharOrFail	-- Trailing quote
		  if mc == '\'' then finish_char_tok loc lit_ch
			        else do setInput i2; lit_error 

        Just (c, i2@(AI _end2 _ _))
		| not (isAny c) -> lit_error
		| otherwise ->

		-- We've seen 'x, where x is a valid character
		--  (i.e. not newline etc) but not a quote or backslash
	   case alexGetChar' i2 of	-- Look ahead one more character
		Just ('\'', i3) -> do 	-- We've seen 'x'
			setInput i3 
			finish_char_tok loc c
		_other -> do 		-- We've seen 'x not followed by quote
		       	  		-- (including the possibility of EOF)
					-- If TH is on, just parse the quote only
			th_exts <- extension thEnabled	
			let (AI end _ _) = i1
			if th_exts then return (L (mkSrcSpan loc end) ITvarQuote)
				   else do setInput i2; lit_error

finish_char_tok :: SrcLoc -> Char -> P (Located Token)
finish_char_tok loc ch	-- We've already seen the closing quote
			-- Just need to check for trailing #
  = do	magicHash <- extension magicHashEnabled
	i@(AI end _ _) <- getInput
	if magicHash then do
		case alexGetChar' i of
			Just ('#',i@(AI end _ _)) -> do
				setInput i
				return (L (mkSrcSpan loc end) (ITprimchar ch))
			_other ->
				return (L (mkSrcSpan loc end) (ITchar ch))
	        else do
		   return (L (mkSrcSpan loc end) (ITchar ch))

lex_char :: Char -> AlexInput -> P Char
lex_char c inp = do
  case c of
      '\\' -> do setInput inp; lex_escape
      c | isAny c -> do setInput inp; return c
      _other -> lit_error

isAny :: Char -> Bool
isAny c | c > '\x7f' = isPrint c
	| otherwise  = is_any c

lex_escape :: P Char
lex_escape = do
  c <- getCharOrFail
  case c of
	'a'   -> return '\a'
	'b'   -> return '\b'
	'f'   -> return '\f'
	'n'   -> return '\n'
	'r'   -> return '\r'
	't'   -> return '\t'
	'v'   -> return '\v'
	'\\'  -> return '\\'
	'"'   -> return '\"'
	'\''  -> return '\''
	'^'   -> do c <- getCharOrFail
		    if c >= '@' && c <= '_'
			then return (chr (ord c - ord '@'))
			else lit_error

	'x'   -> readNum is_hexdigit 16 hexDigit
	'o'   -> readNum is_octdigit  8 octDecDigit
	x | is_decdigit x -> readNum2 is_decdigit 10 octDecDigit (octDecDigit x)

	c1 ->  do
	   i <- getInput
	   case alexGetChar' i of
	    Nothing -> lit_error
	    Just (c2,i2) -> 
              case alexGetChar' i2 of
		Nothing	-> do setInput i2; lit_error
		Just (c3,i3) -> 
		   let str = [c1,c2,c3] in
		   case [ (c,rest) | (p,c) <- silly_escape_chars,
			      	     Just rest <- [stripPrefix p str] ] of
			  (escape_char,[]):_ -> do
				setInput i3
				return escape_char
			  (escape_char,_:_):_ -> do
				setInput i2
				return escape_char
			  [] -> lit_error

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Char
readNum is_digit base conv = do
  i <- getInput
  c <- getCharOrFail
  if is_digit c 
	then readNum2 is_digit base conv (conv c)
	else do setInput i; lit_error

readNum2 :: (Char -> Bool) -> Int -> (Char -> Int) -> Int -> P Char
readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
	  case alexGetChar' input of
	    Just (c,input') | is_digit c -> do
		read (i*base + conv c) input'
	    _other -> do
		if i >= 0 && i <= 0x10FFFF
		   then do setInput input; return (chr i)
		   else lit_error

silly_escape_chars :: [(String, Char)]
silly_escape_chars = [
	("NUL", '\NUL'),
	("SOH", '\SOH'),
	("STX", '\STX'),
	("ETX", '\ETX'),
	("EOT", '\EOT'),
	("ENQ", '\ENQ'),
	("ACK", '\ACK'),
	("BEL", '\BEL'),
	("BS", '\BS'),
	("HT", '\HT'),
	("LF", '\LF'),
	("VT", '\VT'),
	("FF", '\FF'),
	("CR", '\CR'),
	("SO", '\SO'),
	("SI", '\SI'),
	("DLE", '\DLE'),
	("DC1", '\DC1'),
	("DC2", '\DC2'),
	("DC3", '\DC3'),
	("DC4", '\DC4'),
	("NAK", '\NAK'),
	("SYN", '\SYN'),
	("ETB", '\ETB'),
	("CAN", '\CAN'),
	("EM", '\EM'),
	("SUB", '\SUB'),
	("ESC", '\ESC'),
	("FS", '\FS'),
	("GS", '\GS'),
	("RS", '\RS'),
	("US", '\US'),
	("SP", '\SP'),
	("DEL", '\DEL')
	]

-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error :: P a
lit_error = lexError "lexical error in string/character literal"

getCharOrFail :: P Char
getCharOrFail =  do
  i <- getInput
  case alexGetChar' i of
	Nothing -> lexError "unexpected end-of-file in string/character literal"
	Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- QuasiQuote

lex_quasiquote_tok :: Action
lex_quasiquote_tok span buf len = do
  let quoter = reverse $ takeWhile (/= '$')
               $ reverse $ lexemeToString buf (len - 1)
  quoteStart <- getSrcLoc              
  quote <- lex_quasiquote ""
  end <- getSrcLoc 
  return (L (mkSrcSpan (srcSpanStart span) end)
           (ITquasiQuote (mkFastString quoter,
                          mkFastString (reverse quote),
                          mkSrcSpan quoteStart end)))

lex_quasiquote :: String -> P String
lex_quasiquote s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error

    Just ('\\',i)
	| Just ('|',i) <- next -> do 
		setInput i; lex_quasiquote ('|' : s)
	| Just (']',i) <- next -> do 
		setInput i; lex_quasiquote (']' : s)
	where next = alexGetChar' i

    Just ('|',i)
	| Just (']',i) <- next -> do 
		setInput i; return s
	where next = alexGetChar' i

    Just (c, i) -> do
	 setInput i; lex_quasiquote (c : s)

-- -----------------------------------------------------------------------------
-- Warnings

warn :: DynFlag -> SDoc -> Action
warn option warning srcspan _buf _len = do
    addWarning option srcspan warning
    lexToken

warnThen :: DynFlag -> SDoc -> Action -> Action
warnThen option warning action srcspan buf len = do
    addWarning option srcspan warning
    action srcspan buf len

-- -----------------------------------------------------------------------------
-- The Parse Monad

data LayoutContext
  = NoLayout
  | Layout !Int
  deriving Show

data ParseResult a
  = POk PState a
  | PFailed 
	SrcSpan		-- The start and end of the text span related to
			-- the error.  Might be used in environments which can 
			-- show this span, e.g. by highlighting it.
	Message		-- The error message

data PState = PState { 
	buffer	   :: StringBuffer,
        dflags     :: DynFlags,
        messages   :: Messages,
        last_loc   :: SrcSpan,	-- pos of previous token
        last_offs  :: !Int, 	-- offset of the previous token from the
				-- beginning of  the current line.
				-- \t is equal to 8 spaces.
	last_len   :: !Int,	-- len of previous token
        last_line_len :: !Int,
        loc        :: SrcLoc,   -- current loc (end of prev token + 1)
	extsBitmap :: !Int,	-- bitmap that determines permitted extensions
	context	   :: [LayoutContext],
	lex_state  :: [Int]
     }
	-- last_loc and last_len are used when generating error messages,
	-- and in pushCurrentContext only.  Sigh, if only Happy passed the
	-- current token to happyError, we could at least get rid of last_len.
	-- Getting rid of last_loc would require finding another way to 
	-- implement pushCurrentContext (which is only called from one place).

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
	case m s of
		POk s1 a         -> (unP (k a)) s1
		PFailed span err -> PFailed span err

failP :: String -> P a
failP msg = P $ \s -> PFailed (last_loc s) (text msg)

failMsgP :: String -> P a
failMsgP msg = P $ \s -> PFailed (last_loc s) (text msg)

failLocMsgP :: SrcLoc -> SrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \_ -> PFailed (mkSrcSpan loc1 loc2) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg = P $ \_ -> PFailed span msg

getPState :: P PState
getPState = P $ \s -> POk s s

getDynFlags :: P DynFlags
getDynFlags = P $ \s -> POk s (dflags s)

withThisPackage :: (PackageId -> a) -> P a
withThisPackage f
 = do	pkg	<- liftM thisPackage getDynFlags
	return	$ f pkg

extension :: (Int -> Bool) -> P Bool
extension p = P $ \s -> POk s (p $! extsBitmap s)

getExts :: P Int
getExts = P $ \s -> POk s (extsBitmap s)

setExts :: (Int -> Int) -> P ()
setExts f = P $ \s -> POk s{ extsBitmap = f (extsBitmap s) } ()

setSrcLoc :: SrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: SrcSpan -> Int -> Int -> P ()
setLastToken loc len line_len = P $ \s -> POk s { 
  last_loc=loc, 
  last_len=len,
  last_line_len=line_len 
} ()

data AlexInput = AI SrcLoc {-#UNPACK#-}!Int StringBuffer

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ _ buf) = prevChar buf '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AI loc ofs s) 
  | atEnd s   = Nothing
  | otherwise = adj_c `seq` loc' `seq` ofs' `seq` s' `seq` 
		--trace (show (ord c)) $
		Just (adj_c, (AI loc' ofs' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        ofs'   = advanceOffs c ofs

	non_graphic     = '\x0'
	upper 		= '\x1'
	lower		= '\x2'
	digit		= '\x3'
	symbol		= '\x4'
	space		= '\x5'
	other_graphic	= '\x6'

	adj_c 
	  | c <= '\x06' = non_graphic
	  | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
	  | otherwise = 
		case generalCategory c of
		  UppercaseLetter       -> upper
		  LowercaseLetter       -> lower
		  TitlecaseLetter       -> upper
		  ModifierLetter        -> other_graphic
		  OtherLetter           -> lower -- see #1103
		  NonSpacingMark        -> other_graphic
		  SpacingCombiningMark  -> other_graphic
		  EnclosingMark         -> other_graphic
		  DecimalNumber         -> digit
		  LetterNumber          -> other_graphic
		  OtherNumber           -> other_graphic
		  ConnectorPunctuation  -> symbol
		  DashPunctuation       -> symbol
		  OpenPunctuation       -> other_graphic
		  ClosePunctuation      -> other_graphic
		  InitialQuote          -> other_graphic
		  FinalQuote            -> other_graphic
		  OtherPunctuation      -> symbol
		  MathSymbol            -> symbol
		  CurrencySymbol        -> symbol
		  ModifierSymbol        -> symbol
		  OtherSymbol           -> symbol
		  Space                 -> space
		  _other		-> non_graphic

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc ofs s) 
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` ofs' `seq` s' `seq` 
		--trace (show (ord c)) $
		Just (c, (AI loc' ofs' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        ofs'   = advanceOffs c ofs

advanceOffs :: Char -> Int -> Int
advanceOffs '\n' _    = 0
advanceOffs '\t' offs = (offs `quot` 8 + 1) * 8
advanceOffs _    offs = offs + 1

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, last_offs=o, buffer=b } -> POk s (AI l o b)

setInput :: AlexInput -> P ()
setInput (AI l o b) = P $ \s -> POk s{ loc=l, last_offs=o, buffer=b } ()

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

-- for reasons of efficiency, flags indicating language extensions (eg,
-- -fglasgow-exts or -XParr) are represented by a bitmap stored in an unboxed
-- integer

genericsBit :: Int
genericsBit = 0 -- {| and |}
ffiBit :: Int
ffiBit	   = 1
parrBit :: Int
parrBit	   = 2
arrowsBit :: Int
arrowsBit  = 4
thBit :: Int
thBit	   = 5
ipBit :: Int
ipBit      = 6
explicitForallBit :: Int
explicitForallBit = 7 -- the 'forall' keyword and '.' symbol
bangPatBit :: Int
bangPatBit = 8	-- Tells the parser to understand bang-patterns
		-- (doesn't affect the lexer)
tyFamBit :: Int
tyFamBit   = 9	-- indexed type families: 'family' keyword and kind sigs
haddockBit :: Int
haddockBit = 10 -- Lex and parse Haddock comments
magicHashBit :: Int
magicHashBit = 11 -- "#" in both functions and operators
kindSigsBit :: Int
kindSigsBit = 12 -- Kind signatures on type variables
recursiveDoBit :: Int
recursiveDoBit = 13 -- mdo
unicodeSyntaxBit :: Int
unicodeSyntaxBit = 14 -- the forall symbol, arrow symbols, etc
unboxedTuplesBit :: Int
unboxedTuplesBit = 15 -- (# and #)
standaloneDerivingBit :: Int
standaloneDerivingBit = 16 -- standalone instance deriving declarations
transformComprehensionsBit :: Int
transformComprehensionsBit = 17
qqBit :: Int
qqBit	   = 18 -- enable quasiquoting
inRulePragBit :: Int
inRulePragBit = 19
rawTokenStreamBit :: Int
rawTokenStreamBit = 20 -- producing a token stream with all comments included
newQualOpsBit :: Int
newQualOpsBit = 21 -- Haskell' qualified operator syntax, e.g. Prelude.(+)
recBit :: Int
recBit = 22 -- rec

always :: Int -> Bool
always           _     = True
genericsEnabled :: Int -> Bool
genericsEnabled  flags = testBit flags genericsBit
parrEnabled :: Int -> Bool
parrEnabled      flags = testBit flags parrBit
arrowsEnabled :: Int -> Bool
arrowsEnabled    flags = testBit flags arrowsBit
thEnabled :: Int -> Bool
thEnabled        flags = testBit flags thBit
ipEnabled :: Int -> Bool
ipEnabled        flags = testBit flags ipBit
explicitForallEnabled :: Int -> Bool
explicitForallEnabled flags = testBit flags explicitForallBit
bangPatEnabled :: Int -> Bool
bangPatEnabled   flags = testBit flags bangPatBit
-- tyFamEnabled :: Int -> Bool
-- tyFamEnabled     flags = testBit flags tyFamBit
haddockEnabled :: Int -> Bool
haddockEnabled   flags = testBit flags haddockBit
magicHashEnabled :: Int -> Bool
magicHashEnabled flags = testBit flags magicHashBit
-- kindSigsEnabled :: Int -> Bool
-- kindSigsEnabled  flags = testBit flags kindSigsBit
unicodeSyntaxEnabled :: Int -> Bool
unicodeSyntaxEnabled flags = testBit flags unicodeSyntaxBit
unboxedTuplesEnabled :: Int -> Bool
unboxedTuplesEnabled flags = testBit flags unboxedTuplesBit
standaloneDerivingEnabled :: Int -> Bool
standaloneDerivingEnabled flags = testBit flags standaloneDerivingBit
qqEnabled :: Int -> Bool
qqEnabled        flags = testBit flags qqBit
-- inRulePrag :: Int -> Bool
-- inRulePrag       flags = testBit flags inRulePragBit
rawTokenStreamEnabled :: Int -> Bool
rawTokenStreamEnabled flags = testBit flags rawTokenStreamBit
newQualOps :: Int -> Bool
newQualOps       flags = testBit flags newQualOpsBit
oldQualOps :: Int -> Bool
oldQualOps flags = not (newQualOps flags)

-- PState for parsing options pragmas
--
pragState :: DynFlags -> StringBuffer -> SrcLoc -> PState
pragState dynflags buf loc =
  PState {
      buffer        = buf,
      messages      = emptyMessages,
      dflags        = dynflags,
      last_loc      = mkSrcSpan loc loc,
      last_offs     = 0,
      last_len      = 0,
      last_line_len = 0,
      loc           = loc,
      extsBitmap    = 0,
      context       = [],
      lex_state     = [bol, option_prags, 0]
    }


-- create a parse state
--
mkPState :: StringBuffer -> SrcLoc -> DynFlags -> PState
mkPState buf loc flags  = 
  PState {
      buffer	      = buf,
      dflags        = flags,
      messages      = emptyMessages,
      last_loc      = mkSrcSpan loc loc,
      last_offs     = 0,
      last_len      = 0,
      last_line_len = 0,
      loc           = loc,
      extsBitmap    = fromIntegral bitmap,
      context       = [],
      lex_state     = [bol, 0]
	-- we begin in the layout state if toplev_layout is set
    }
    where
      bitmap = genericsBit `setBitIf` dopt Opt_Generics flags
	       .|. ffiBit            `setBitIf` dopt Opt_ForeignFunctionInterface flags
	       .|. parrBit           `setBitIf` dopt Opt_PArr         flags
	       .|. arrowsBit         `setBitIf` dopt Opt_Arrows       flags
	       .|. thBit             `setBitIf` dopt Opt_TemplateHaskell flags
	       .|. qqBit             `setBitIf` dopt Opt_QuasiQuotes flags
	       .|. ipBit             `setBitIf` dopt Opt_ImplicitParams flags
	       .|. explicitForallBit `setBitIf` dopt Opt_ExplicitForAll flags
	       .|. bangPatBit        `setBitIf` dopt Opt_BangPatterns flags
	       .|. tyFamBit          `setBitIf` dopt Opt_TypeFamilies flags
	       .|. haddockBit        `setBitIf` dopt Opt_Haddock      flags
	       .|. magicHashBit      `setBitIf` dopt Opt_MagicHash    flags
	       .|. kindSigsBit       `setBitIf` dopt Opt_KindSignatures flags
	       .|. recursiveDoBit    `setBitIf` dopt Opt_RecursiveDo flags
	       .|. recBit 	     `setBitIf` dopt Opt_DoRec  flags
	       .|. recBit 	     `setBitIf` dopt Opt_Arrows flags
	       .|. unicodeSyntaxBit  `setBitIf` dopt Opt_UnicodeSyntax flags
	       .|. unboxedTuplesBit  `setBitIf` dopt Opt_UnboxedTuples flags
	       .|. standaloneDerivingBit `setBitIf` dopt Opt_StandaloneDeriving flags
               .|. transformComprehensionsBit `setBitIf` dopt Opt_TransformListComp flags
               .|. rawTokenStreamBit `setBitIf` dopt Opt_KeepRawTokenStream flags
               .|. newQualOpsBit `setBitIf` dopt Opt_NewQualifiedOperators flags
      --
      setBitIf :: Int -> Bool -> Int
      b `setBitIf` cond | cond      = bit b
			| otherwise = 0

addWarning :: DynFlag -> SrcSpan -> SDoc -> P ()
addWarning option srcspan warning
 = P $ \s@PState{messages=(ws,es), dflags=d} ->
       let warning' = mkWarnMsg srcspan alwaysQualify warning
           ws' = if dopt option d then ws `snocBag` warning' else ws
       in POk s{messages=(ws', es)} ()

getMessages :: PState -> Messages
getMessages PState{messages=ms} = ms

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, context = ctx, 
                              last_len = len, last_loc = last_loc }) ->
  case ctx of
	(_:tl) -> POk s{ context = tl } ()
	[]     -> PFailed last_loc (srcParseErr buf len)

-- Push a new layout context at the indentation of the last token read.
-- This is only used at the outer level of a module when the 'module'
-- keyword is missing.
pushCurrentContext :: P ()
pushCurrentContext = P $ \ s@PState{ last_offs=offs, last_line_len=len, context=ctx } -> 
    POk s{context = Layout (offs-len) : ctx} ()
--trace ("off: " ++ show offs ++ ", len: " ++ show len) $ POk s{context = Layout (offs-len) : ctx} ()

getOffside :: P Ordering
getOffside = P $ \s@PState{last_offs=offs, context=stk} ->
		let ord = case stk of
			(Layout n:_) -> compare offs n
			_            -> GT
		in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: StringBuffer	-- current buffer (placed just after the last token)
  -> Int		-- length of the previous token
  -> Message
srcParseErr buf len
  = hcat [ if null token 
	     then ptext (sLit "parse error (possibly incorrect indentation)")
	     else hcat [ptext (sLit "parse error on input "),
          	  	char '`', text token, char '\'']
    ]
  where token = lexemeToString (offsetBytes (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len, 	
			    last_loc = last_loc } ->
    PFailed last_loc (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: String -> P a
lexError str = do
  loc <- getSrcLoc
  (AI end _ buf) <- getInput
  reportLexError loc end buf str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  tok@(L _span _tok__) <- lexToken
--  trace ("token: " ++ show tok__) $ do
  cont tok

lexToken :: P (Located Token)
lexToken = do
  inp@(AI loc1 _ buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do
        let span = mkSrcSpan loc1 loc1
        setLastToken span 0 0
        return (L span ITeof)
    AlexError (AI loc2 _ buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end _ buf2) _ t -> do
        setInput inp2
        let span = mkSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes bytes
        t span buf bytes

reportLexError :: SrcLoc -> SrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let 
	c = fst (nextChar buf)
  in
  if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
    then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
    else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)

lexTokenStream :: StringBuffer -> SrcLoc -> DynFlags -> ParseResult [Located Token]
lexTokenStream buf loc dflags = unP go initState
    where initState = mkPState buf loc (dopt_set (dopt_unset dflags Opt_Haddock) Opt_KeepRawTokenStream)
          go = do
            ltok <- lexer return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go

linePrags = Map.singleton "line" (begin line_prag2)

fileHeaderPrags = Map.fromList([("options", lex_string_prag IToptions_prag),
                                 ("options_ghc", lex_string_prag IToptions_prag),
                                 ("options_haddock", lex_string_prag ITdocOptions),
                                 ("language", token ITlanguage_prag),
                                 ("include", lex_string_prag ITinclude_prag)])

ignoredPrags = Map.fromList (map ignored pragmas)
               where ignored opt = (opt, nested_comment lexToken)
                     impls = ["hugs", "nhc98", "jhc", "yhc", "catch", "derive"]
                     options_pragmas = map ("options_" ++) impls
                     -- CFILES is a hugs-only thing.
                     pragmas = options_pragmas ++ ["cfiles", "contract"]

oneWordPrags = Map.fromList([("rules", rulePrag),
                           ("inline", token (ITinline_prag True)),
                           ("notinline", token (ITinline_prag False)),
                           ("specialize", token ITspec_prag),
                           ("source", token ITsource_prag),
                           ("warning", token ITwarning_prag),
                           ("deprecated", token ITdeprecated_prag),
                           ("scc", token ITscc_prag),
                           ("generated", token ITgenerated_prag),
                           ("core", token ITcore_prag),
                           ("unpack", token ITunpack_prag),
                           ("ann", token ITann_prag)])

twoWordPrags = Map.fromList([("inline conlike", token (ITinline_conlike_prag True)),
                             ("notinline conlike", token (ITinline_conlike_prag False)),
                             ("specialize inline", token (ITspec_inline_prag True)),
                             ("specialize notinline", token (ITspec_inline_prag False))])


dispatch_pragmas :: Map String Action -> Action
dispatch_pragmas prags span buf len = case Map.lookup (clean_pragma (lexemeToString buf len)) prags of
                                       Just found -> found span buf len
                                       Nothing -> lexError "unknown pragma"

known_pragma :: Map String Action -> AlexAccPred Int
known_pragma prags _ _ len (AI _ _ buf) = (isJust $ Map.lookup (clean_pragma (lexemeToString (offsetBytes (- len) buf) len)) prags)
                                          && (nextCharIs buf (\c -> not (isAlphaNum c || c == '_')))

clean_pragma :: String -> String
clean_pragma prag = canon_ws (map toLower (unprefix prag))
                    where unprefix prag' = case stripPrefix "{-#" prag' of
                                             Just rest -> rest
                                             Nothing -> prag'
                          canonical prag' = case prag' of
                                              "noinline" -> "notinline"
                                              "specialise" -> "specialize"
                                              "constructorlike" -> "conlike"
                                              otherwise -> prag'
                          canon_ws s = unwords (map canonical (words s))
}
