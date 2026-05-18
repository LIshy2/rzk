{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rzk.VSCode.Tokenize where

import           Language.LSP.Protocol.Types (SemanticTokenAbsolute (..),
                                              SemanticTokenModifiers (..),
                                              SemanticTokenTypes (..))
import           Language.Rzk.Syntax

tokenizeModule :: Module -> [SemanticTokenAbsolute]
tokenizeModule (Module _loc langDecl commands) = concat
  [ tokenizeLanguageDecl langDecl
  , foldMap tokenizeCommand commands
  ]

tokenizeLanguageDecl :: LanguageDecl -> [SemanticTokenAbsolute]
tokenizeLanguageDecl ld@(LanguageDecl _ _) = mkKeyword ld "#lang"

tokenizeCommand :: Command -> [SemanticTokenAbsolute]
tokenizeCommand command = case command of
  cmd@CommandSetOption{}   -> mkKeyword cmd "#set-option"
  cmd@CommandUnsetOption{} -> mkKeyword cmd "#unset-option"

  cmd@(CommandCheck _ term type_) -> concat
    [ mkKeyword cmd "#check"
    , foldMap tokenizeTerm [term, type_]
    ]
  cmd@(CommandCompute _ term) -> concat
    [ mkKeyword cmd "#compute"
    , tokenizeTerm term
    ]
  cmd@(CommandComputeNF _ term) -> concat
    [ mkKeyword cmd "#compute-nf"
    , tokenizeTerm term
    ]
  cmd@(CommandComputeWHNF _ term) -> concat
    [ mkKeyword cmd "#compute-whnf"
    , tokenizeTerm term
    ]

  cmd@(CommandPostulate _ name _declUsedVars params type_) -> concat
    [ mkKeyword cmd "#postulate"
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , tokenizeTerm type_
    ]
  cmd@(CommandDefine _ name _declUsedVars params type_ term) -> concat
    [ mkKeyword cmd "#def"
    , mkToken name SemanticTokenTypes_Function [SemanticTokenModifiers_Declaration]
    , foldMap tokenizeParam params
    , foldMap tokenizeTerm [type_, term]
    ]

  cmd@(CommandAssume _ vars type_) -> concat
    [ mkKeyword cmd "#assume"
    , foldMap (\var -> mkToken var SemanticTokenTypes_Parameter []) vars
    , tokenizeTerm type_
    ]
  cmd@CommandSection{}    -> mkKeyword cmd "#section"
  cmd@CommandSectionEnd{} -> mkKeyword cmd "#end"

tokenizeBind :: Bind -> [SemanticTokenAbsolute]
tokenizeBind = \case
  BindPattern _loc pat -> tokenizePattern pat
  BindPatternType _loc pat type_ -> concat [tokenizePattern pat, tokenizeTerm type_]

tokenizeParam :: Param -> [SemanticTokenAbsolute]
tokenizeParam = \case
  ParamPattern _loc pat -> tokenizePattern pat
  ParamPatternType _loc pats type_ -> concat
    [ foldMap tokenizePattern pats
    , tokenizeTerm type_ ]
  ParamPatternShape _loc pats cube tope -> concat
    [ foldMap tokenizePattern pats
    , tokenizeTerm cube
    , tokenizeTope tope ]
  ParamPatternShapeDeprecated _loc pat cube tope -> concat
    [ tokenizePattern pat
    , tokenizeTerm cube
    , tokenizeTope tope ]
  ParamPatternModalType _loc pats md ty -> concat
    [ foldMap tokenizePattern pats
    , tokenizeModality md
    , tokenizeTerm ty ]

tokenizePattern :: Pattern -> [SemanticTokenAbsolute]
tokenizePattern = \case
  PatternVar _loc var    -> mkToken var SemanticTokenTypes_Parameter [SemanticTokenModifiers_Declaration]
  PatternPair _loc l r   -> foldMap tokenizePattern [l, r]
  pat@(PatternUnit _loc) -> mkToken pat SemanticTokenTypes_EnumMember [SemanticTokenModifiers_Declaration]
  PatternTuple _loc p1 p2 ps -> foldMap tokenizePattern (p1 : p2 : ps)

tokenizeTope :: Term -> [SemanticTokenAbsolute]
tokenizeTope = tokenizeTerm' (Just SemanticTokenTypes_String)

tokenizeTerm :: Term -> [SemanticTokenAbsolute]
tokenizeTerm = tokenizeTerm' Nothing

tokenizeTerm' :: Maybe SemanticTokenTypes -> Term -> [SemanticTokenAbsolute]
tokenizeTerm' varTokenType = go
  where
    go term = case term of
      Hole{} -> [] -- FIXME
      Var{} -> case varTokenType of
                 Nothing         -> []
                 Just token_type -> mkToken term token_type []

      Universe{}           -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
      UniverseCube{}       -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]
      UniverseTope{}       -> mkToken term SemanticTokenTypes_Class [SemanticTokenModifiers_DefaultLibrary]

      CubeUnit{}           -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      CubeUnitStar{}       -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_CubeUnitStar{} -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      Cube2{}              -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      Cube2_0{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_Cube2_0{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      Cube2_1{}            -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]
      ASCII_Cube2_1{}      -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      CubeProduct _loc l r -> foldMap go [l, r]

      TopeTop{}            -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeTop{}      -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeBottom{}         -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      ASCII_TopeBottom{}   -> mkToken term SemanticTokenTypes_String [SemanticTokenModifiers_DefaultLibrary]
      TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeAnd _loc l r     -> foldMap tokenizeTope [l, r]
      TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeOr  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeEQ  _loc l r     -> foldMap tokenizeTope [l, r]
      TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]
      ASCII_TopeLEQ _loc l r     -> foldMap tokenizeTope [l, r]
      TopeInv _loc t       -> foldMap tokenizeTope [t]
      TopeUninv _loc t     -> foldMap tokenizeTope [t]
      CubeFlip _loc c      -> foldMap go [c]
      CubeUnflip _loc c    -> foldMap go [c]

      RecBottom{}          -> mkToken term SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
      RecOr _loc rs -> foldMap tokenizeRestriction rs

      TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      ASCII_TypeFun _loc paramDecl ret -> concat
        [ tokenizeParamDecl paramDecl
        , go ret ]
      TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      TypeSigmaModal loc pat md a b -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , tokenizeModality md
        , foldMap go [a, b] ]
      ASCII_TypeSigma loc pat a b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , foldMap go [a, b] ]
      ASCII_TypeSigmaModal loc pat md a b -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , tokenizePattern pat
        , tokenizeModality md
        , foldMap go [a, b] ]
      TypeSigmaTuple loc p ps tN -> concat
        [ mkToken (VarIdent loc "∑") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , foldMap tokenizeSigmaParam (p : ps)
        , go tN ]
      ASCII_TypeSigmaTuple loc p ps tN -> concat
        [ mkToken (VarIdent loc "Sigma") SemanticTokenTypes_Type [SemanticTokenModifiers_DefaultLibrary]
        , foldMap tokenizeSigmaParam (p : ps)
        , go tN ]
      TypeId _loc x a y -> foldMap go [x, a, y]
      TypeIdSimple _loc x y -> foldMap go [x, y]
      TypeRestricted _loc type_ rs -> concat
        [ go type_
        , foldMap tokenizeRestriction rs ]

      App _loc f x -> foldMap go [f, x]
      Lambda _loc params body -> concat
        [ foldMap tokenizeParam params
        , go body ]
      t@(Let _ bind val expr) -> concat
        [ mkKeyword t "let"
        , tokenizeBind bind
        , go val
        , go expr ]
      t@(LetMod _ comp bind val expr) -> concat
        [ mkKeyword t "let"
        , mkModifierBefore comp "mod"
        , tokenizeModComp comp
        , tokenizeBind bind
        , go val
        , go expr ]
      ASCII_Lambda loc params body -> go (Lambda loc params body)

      Pair _loc l r -> foldMap go [l, r]
      Tuple _loc p1 p2 ps -> foldMap go (p1:p2:ps)
      First loc t -> concat
        [ mkToken (VarIdent loc "π₁") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      ASCII_First loc t -> concat
        [ mkToken (VarIdent loc "first") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      Second loc t -> concat
        [ mkToken (VarIdent loc "π₂") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]
      ASCII_Second loc t -> concat
        [ mkToken (VarIdent loc "second") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go t ]

      TypeUnit _loc -> mkToken term SemanticTokenTypes_Enum [SemanticTokenModifiers_DefaultLibrary]
      Unit _loc -> mkToken term SemanticTokenTypes_EnumMember [SemanticTokenModifiers_DefaultLibrary]

      Refl{} -> mkToken term SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
      ReflTerm loc x -> concat
        [ mkToken (VarIdent loc "refl") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , go x ]
      ReflTermType loc x a -> concat
        [ mkToken (VarIdent loc "refl") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , foldMap go [x, a] ]

      IdJ loc a b c d e f -> concat
        [ mkToken (VarIdent loc "J") SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]
        , foldMap go [a, b, c, d, e, f] ]

      TypeAsc _loc t type_ -> foldMap go [t, type_]

      ModType _loc md type_ -> tokenizeModality md <> go type_
      ModApp  _loc md te    -> tokenizeModality md <> go te
      ModExtract _loc comp te -> tokenizeModComp comp <> go te

      RecOrDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]
      ASCII_TypeExtensionDeprecated{} -> mkToken term SemanticTokenTypes_Regexp [SemanticTokenModifiers_Deprecated]


tokenizeRestriction :: Restriction -> [SemanticTokenAbsolute]
tokenizeRestriction (Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]
tokenizeRestriction (ASCII_Restriction _loc tope term) = concat
  [ tokenizeTope tope
  , tokenizeTerm term ]

tokenizeParamDecl :: ParamDecl -> [SemanticTokenAbsolute]
tokenizeParamDecl = \case
  ParamType _loc type_ -> tokenizeTerm type_
  ParamTermType _loc pat type_ -> concat
    [ tokenizeTerm pat
    , tokenizeTerm type_ ]
  ParamTermShape _loc pat cube tope -> concat
    [ tokenizeTerm pat
    , tokenizeTerm cube
    , tokenizeTope tope
    ]
  ParamTermTypeDeprecated _loc pat type_ -> concat
    [ tokenizePattern pat
    , tokenizeTerm type_ ]
  ParamVarShapeDeprecated _loc pat cube tope -> concat
    [ tokenizePattern pat
    , tokenizeTerm cube
    , tokenizeTope tope
    ]
  ParamTermModalType _loc pat md type_ -> concat
    [ tokenizeTerm pat
    , tokenizeModality md
    , tokenizeTerm type_ ]

tokenizeSigmaParam :: SigmaParam -> [SemanticTokenAbsolute]
tokenizeSigmaParam = \case
  SigmaParam _loc pat type_ -> concat
    [ tokenizePattern pat
    , tokenizeTerm type_ ]
  SigmaParamModal _loc pat md type_ -> concat
    [ tokenizePattern pat
    , tokenizeModality md
    , tokenizeTerm type_ ]

tokenizeModality :: Modality -> [SemanticTokenAbsolute]
tokenizeModality m = mkToken m SemanticTokenTypes_Modifier []

tokenizeModComp :: ModComp -> [SemanticTokenAbsolute]
tokenizeModComp = \case
  Single _loc m      -> tokenizeModality m
  Comp   _loc m1 m2  -> tokenizeModality m1 <> tokenizeModality m2

mkKeyword :: (HasPosition a) => a -> String -> [SemanticTokenAbsolute]
mkKeyword x kw = case hasPosition x of
  Nothing -> []
  Just (line, col) ->
    [ SemanticTokenAbsolute
      { _tokenType      = SemanticTokenTypes_Keyword
      , _tokenModifiers = []
      , _line           = fromIntegral line - 1
      , _startChar      = fromIntegral col  - 1
      , _length         = fromIntegral (length kw)
      } ]

mkModifierBefore :: (HasPosition a) => a -> String -> [SemanticTokenAbsolute]
mkModifierBefore x kw = case hasPosition x of
  Nothing -> []
  Just (line, col) ->
    [ SemanticTokenAbsolute
      { _tokenType      = SemanticTokenTypes_Modifier
      , _tokenModifiers = []
      , _line           = fromIntegral line - 1
      , _startChar      = fromIntegral col - 1 - fromIntegral (length kw + 1)
      , _length         = fromIntegral (length kw)
      } ]


mkToken :: (HasPosition a, Print a) => a -> SemanticTokenTypes -> [SemanticTokenModifiers] -> [SemanticTokenAbsolute]
mkToken x tokenType tokenModifiers =
  case hasPosition x of
    Nothing -> []
    Just (line, col) -> do
      [ SemanticTokenAbsolute
        { _tokenType = tokenType
        , _tokenModifiers = tokenModifiers
        , _startChar = fromIntegral col - 1
        ,  _line = fromIntegral line - 1
        ,  _length = fromIntegral $ Prelude.length (printTree x)
        }
        ]
