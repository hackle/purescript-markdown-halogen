module Text.Markdown.SlamDown.Halogen.Component.State
  ( FormFieldValue
  , SlamDownFormDesc
  , SlamDownFormState
  , SlamDownStateR
  , SlamDownState(..)

  , emptySlamDownState

  , getDocument

  , syncState
  , replaceDocument

  , formDescFromDocument
  , formStateFromDocument

  ) where

import Prelude

import Data.Const (Const(..))
import Data.Foldable as F
import Data.Identity as Id
import Data.List as L
import Data.Maybe as M
import Data.Monoid (mempty)
import Foreign.Object as SM
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup as V

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary as SCA

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Parser.Inline as SDPI
import Text.Markdown.SlamDown.Traverse as SDT

import Foreign.Object

type FormFieldValue = SD.FormFieldP Id.Identity
type SlamDownFormDesc a = Object (SD.FormField a)
type SlamDownFormState a = Object (FormFieldValue a)

type SlamDownStateR a =
  { document ∷ SD.SlamDownP a
  , formState ∷ SlamDownFormState a
  }

-- | The state of a SlamDown form
newtype SlamDownState a = SlamDownState (SlamDownStateR a)

instance functorSlamDownState ∷ Functor SlamDownState where
  map f (SlamDownState st) =
    SlamDownState
      { document: f <$> st.document
      , formState: map f <$> st.formState
      }

getDocument ∷ SlamDownState ~> SD.SlamDownP
getDocument (SlamDownState rec) = rec.document

instance showSlamDownState ∷ (Show a) ⇒ Show (SlamDownState a) where
  show (SlamDownState rec) = "(SlamDownState " <> show rec.formState <> ")"

formStateFromDocument ∷ SD.SlamDownP ~> SlamDownFormState
formStateFromDocument =
  SM.fromFoldable
    <<< SDT.everything (const mempty) phi
  where
    phi
      ∷ ∀ v
      . SD.Inline v
      → L.List (Tuple String (FormFieldValue v))
    phi (SD.FormField label _ field) =
      M.maybe mempty (L.singleton <<< Tuple label) $
        V.unV (const M.Nothing) M.Just (SDPI.validateFormField field)
          >>= formFieldGetDefaultValue
    phi _ = mempty

formFieldGetDefaultValue
  ∷ ∀ v
  . SD.FormField v
  → M.Maybe (FormFieldValue v)
formFieldGetDefaultValue =
  SD.traverseFormField (SD.getLiteral >>> map pure)

-- | The initial empty state of the form, with an empty document.
emptySlamDownState ∷ ∀ v. SlamDownState v
emptySlamDownState =
  SlamDownState
    { document : SD.SlamDown mempty
    , formState : SM.empty
    }

-- | The initial state of the form based on a document value. All fields use
-- | their default values.
makeSlamDownState ∷ SD.SlamDownP ~> SlamDownState
makeSlamDownState doc =
  SlamDownState
    { document : doc
    , formState : formStateFromDocument doc
    }


formDescFromDocument ∷ SD.SlamDownP ~> SlamDownFormDesc
formDescFromDocument =
  SM.fromFoldable
    <<< SDT.everything (const mempty) phi
  where
    phi ∷ ∀ v. SD.Inline v → L.List (Tuple String (SD.FormField v))
    phi (SD.FormField label _ field) = L.singleton (Tuple label field)
    phi _ = mempty

syncState
  ∷ ∀ v
  . SD.Value v
  ⇒ SD.SlamDownP v
  → SlamDownFormState v
  → SlamDownState v
syncState doc formState =
  SlamDownState
    { document: doc
    , formState: formState'
    }
  where
    formDesc ∷ SlamDownFormDesc v
    formDesc = formDescFromDocument doc

    eraseTextBox ∷ ∀ f. SD.TextBox f → SD.TextBox (Const Unit)
    eraseTextBox = SD.transTextBox \_ → Const unit

    -- | Returns the keys that are either not present in the new state, or have had their types changed.
    keysToPrune ∷ SlamDownFormState v → Array String
    keysToPrune =
      SM.foldMap \key oldVal →
        case SM.lookup key formDesc of
          M.Nothing → [ key ]
          M.Just formVal →
            case oldVal, formVal of
              SD.TextBox tb1, SD.TextBox tb2 | eraseTextBox tb1 == eraseTextBox tb2 → []
              SD.CheckBoxes _ (Id.Identity xs1), SD.CheckBoxes _ (SD.Literal xs2) | xs1 == xs2 → []
              SD.DropDown _ (Id.Identity xs1), SD.DropDown _ (SD.Literal xs2) | xs1 == xs2 → []
              SD.RadioButtons _ (Id.Identity xs1), SD.RadioButtons _ (SD.Literal xs2) | xs1 == xs2 → []
              _, _ → [ key ]

    formState' ∷ SlamDownFormState v
    formState' = F.foldr SM.delete formState $ keysToPrune formState

replaceDocument
  ∷ ∀ v
  . (SD.Value v)
  ⇒ SD.SlamDownP v
  → SlamDownState v
  → SlamDownState v
replaceDocument doc (SlamDownState state) =
  syncState doc state.formState
