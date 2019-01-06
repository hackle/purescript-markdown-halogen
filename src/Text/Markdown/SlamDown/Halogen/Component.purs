-- | This module defines a component for rendering SlamDown documents to HTML
module Text.Markdown.SlamDown.Halogen.Component (
  slamDownComponent
  , renderSlamDown
  , module SDS
  , module SDQ
  ) where

import Prelude

import DOM.HTML.Indexed.StepValue (StepValue)
import Data.Array as A
import Data.Either.Nested (Either3)
import Data.Foldable as F
import Data.Identity (Identity)
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Text.Markdown.SlamDown (CodeBlockType(..))
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.Query (SlamDownQuery(..)) as SDQ
import Text.Markdown.SlamDown.Halogen.Component.State (FormFieldValue, SlamDownFormDesc, SlamDownFormState, SlamDownState(..), SlamDownStateR, emptySlamDownState, formDescFromDocument, formStateFromDocument, getDocument, replaceDocument, syncState) as SDS
import Text.Markdown.SlamDown.Halogen.Fresh as Fresh


type FreshRenderer v m a = a → Fresh.Fresh (HTML v m)

-- | Render a `SlamDown` document into an HTML form.
renderSlamDown
  ∷ ∀ v m
  . SD.Value v
  => SDS.SlamDownState v
  → HTML v m
renderSlamDown (SDS.SlamDownState state) =
  case state.document of
    SD.SlamDown bs →
      HH.div_
        $ Fresh.runFresh "form_"
        $ renderBlocks bs

  where
    defaultFormState ∷ SDS.SlamDownFormState v
    defaultFormState = SDS.formStateFromDocument state.document

    h_ ∷ Int → Array (HTML v m) → HTML v m
    h_ 1 = HH.h1_
    h_ 2 = HH.h2_
    h_ 3 = HH.h3_
    h_ 4 = HH.h4_
    h_ 5 = HH.h5_
    h_ _ = HH.h6_

    el_ ∷ SD.ListType → Array (HTML v m) → HTML v m
    el_ (SD.Bullet _)  = HH.ul_
    el_ (SD.Ordered _) = HH.ol_

    renderInline ∷ FreshRenderer v m (SD.Inline v)
    renderInline i =
      case i of
        SD.Str s → pure $ HH.text s
        SD.Entity s → pure $ HH.text s
        SD.Space → pure $ HH.text " "
        SD.FormField _ _ _ -> pure $ HH.text ""
        SD.SoftBreak → pure $ HH.text "\n"
        SD.LineBreak → pure $ HH.br_
        SD.Emph is → HH.em_ <$> traverse renderInline (A.fromFoldable is)
        SD.Strong is → HH.strong_ <$> traverse renderInline (A.fromFoldable is)
        SD.Code _ c → pure $ HH.code_ [ HH.text c ]
        SD.Link body tgt → do
          let
            href (SD.InlineLink url) = url
            href (SD.ReferenceLink tgt') = M.maybe "" ("#" <> _) tgt'
          HH.a [ HP.href $ href tgt ] <$> traverse renderInline (A.fromFoldable body)
        SD.Image body url →
          pure $ HH.img
            [ HP.src url
            , HP.alt $ F.foldMap stripInline body
            ]

    stripInline ∷ SD.Inline v → String
    stripInline i =
      case i of
        SD.Str s → s
        SD.Entity s → s
        SD.Space → " "
        SD.SoftBreak → "\n"
        SD.LineBreak → "\n"
        SD.Emph is → F.foldMap stripInline is
        SD.Strong is → F.foldMap stripInline is
        SD.Code _ c → c
        SD.Link body _ → F.foldMap stripInline body
        _ → ""

    renderBlock ∷ FreshRenderer v m (SD.Block v)
    renderBlock b =
      case b of
        SD.Paragraph is →
          HH.p_ <$> traverse renderInline (A.fromFoldable is)
        SD.Header lvl is →
          h_ lvl <$> traverse renderInline (A.fromFoldable is)
        SD.Blockquote bs →
          HH.blockquote_ <$> renderBlocks bs
        SD.Lst lt bss → do
          let
            item ∷ FreshRenderer v m (L.List (SD.Block v))
            item bs = HH.li_ <$> renderBlocks bs
          el_ lt <$> traverse item (A.fromFoldable bss)
        SD.CodeBlock cbType ss →
          pure $ HH.pre
            [ HP.class_ $ HH.ClassName $
                case cbType of
                Indented -> ""
                Fenced _ l -> "language-" <> l
            ]
            [ HH.code_ [ HH.text (S.joinWith "\n" $ A.fromFoldable ss) ] ]
        SD.LinkReference l url →
          pure $ HH.p_
            [ HH.text (l <> ": ")
            , HH.a [ HP.id_ l, HP.href url ] [ HH.text url ]
            ]
        SD.Rule →
          pure HH.hr_

    renderBlocks ∷ L.List (SD.Block v) → Fresh.Fresh (Array (HTML v m))
    renderBlocks = go [] L.Nil
      where
      go html fs bs =
        case bs of
          L.Cons (SD.Paragraph (L.Cons (SD.FormField label required field) L.Nil)) bs' →
            go html (L.Cons { label, required, field } fs) bs'
          L.Cons b bs' → do
            bHtml ← renderBlock b
            go (html <> pure bHtml) L.Nil bs'
          L.Nil → pure html

dateFormatString :: String
dateFormatString = "YYYY-MMMM-DD"

timeFormatString :: SD.TimePrecision → String
timeFormatString SD.Minutes = "HH:mm"
timeFormatString SD.Seconds = "HH:mm:ss"

step ∷ ∀ r i. StepValue → HP.IProp (step ∷ StepValue | r) i
step = HP.prop (HC.PropName "step")

type Slot = Either3 String String String

type HTML v m = H.ParentHTML (SDQ.SlamDownQuery v) Identity Slot m
type DSL v m = H.ParentDSL (SDS.SlamDownState v) (SDQ.SlamDownQuery v) Identity Slot Void m

evalSlamDownQuery
  ∷ ∀ m v
  . (SD.Value v)
  => SDQ.SlamDownQuery v
  ~> DSL v m
evalSlamDownQuery e =
  case e of
    SDQ.SetDocument doc next → do
      state ← H.get
      let newState = SDS.replaceDocument doc state
      H.put newState
      pure next

-- | Bundles up the SlamDown renderer and state machine into a Halogen component.
slamDownComponent
  ∷ ∀ m v
  . SD.Value v
  => H.Component HH.HTML (SDQ.SlamDownQuery v) Unit Void m
slamDownComponent =
  H.parentComponent
    { render: renderSlamDown
    , eval: evalSlamDownQuery
    , initialState: const SDS.emptySlamDownState
    , receiver: const M.Nothing
    }