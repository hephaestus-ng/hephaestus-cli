module ConfigTypes where



data ParserError = PathError | FormatError

data AssetError = ParserT ParserError | ParserA ParserError


data SPLError = FeatureModel | ConfigurationKnowledge


data HephaestusError = SPLError | AssetError


newtype App a =
  App {
    unApp :: ReaderT HephaestusConfig (ExceptT AppError IO) a
  } deriving (
    Functor, Applicative, Monad,
    MonadReader HephaestusConfig,
    MonadError HephaestusError,
    MonadIO
  )



type Path = String

data HephaestusConfig =
  HephaestusConfig {
    assetConfig :: AssetConfig
    ckConfig    :: CKConfig
  }

type AssetConfig = Path

type CKConfig = Path
