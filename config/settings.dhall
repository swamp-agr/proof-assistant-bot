let Limit = { soft : Natural, hard : Natural }
let ExternalSettings =
      { args : List Text
      , packages : Text
      , executable : Text
      , time : Natural
      , priority : Natural
      , resources :
          { totalMemory : Limit
          , dataSize : Limit
          , openFiles : Limit
          , fileSize : Limit
          , cpuTime : Limit
          }
      , fileExtension : Text
      , inputSize : Natural
      , tempFilePrefix : Text
      }
let InternalSettings =
      { timeout : Natural
      , allocations : Natural
      , inputSize : Natural
      , sourceFilePrefix : Text
      , sourceFileExtension : Text
      }
let AgdaSettings = { internal : InternalSettings }
let defaultResource =
      { totalMemory = { soft = 5, hard = 10 }
      , dataSize = { soft = 2176782336, hard = 2176782336 }
      , openFiles = { soft = 7, hard = 7 }
      , fileSize = { soft = 10800, hard = 10800 }
      , cpuTime = { soft = 4, hard = 5 }
      }
let emptyExternalSettings =
      { args = [] : List Text
      , packages = ""
      , executable = ""
      , time = 5
      , priority = 20
      , resources = defaultResource
      , fileExtension = ""
      , inputSize = 1000000
      , tempFilePrefix = ""
      } : ExternalSettings
let emptyInternalSettings =
      { timeout = 10
      , allocations = 20000000000
      , inputSize = 1000000
      , sourceFilePrefix = ""
      , sourceFileExtension = ""
      } : InternalSettings
let coqSettings = emptyExternalSettings //
      { args = ["-quiet"] : List Text
      , executable = env:COQ_BIN_PATH as Text
      , fileExtension = "v"
      , tempFilePrefix = "coq"
      } : ExternalSettings
let idrisSettings = emptyExternalSettings //
      { args = ["--no-banner", "--no-color", "--client"] : List Text
      , executable = env:IDRIS2_BIN_PATH as Text
      , tempFilePrefix = "idris"
      , fileExtension = "idr"
      }
let Settings = { botName : Text
               , allowedCommands : List Text
               , botToken : Text
               , interpretersSettings : { agda : AgdaSettings
                                        , arend : ExternalSettings 
                                        , idris : ExternalSettings 
                                        , coq : ExternalSettings 
                                        , lean : ExternalSettings 
                                        , rzk : InternalSettings
                                        }
               , outputSize : Natural
               }
let interpreterSettings =
      { agda =
          { internal = emptyInternalSettings
            // { sourceFilePrefix = "agda"
               , sourceFileExtension = "agda"
               }
          }
      , arend = emptyExternalSettings
      , idris = idrisSettings
      , coq = coqSettings
      , lean = emptyExternalSettings
      , rzk  = emptyInternalSettings
      }

in

{ botName = "ProofAssistantBot"
, allowedCommands =
    [ "/coq", "coq" ]
, botToken = env:PROOF_ASSISTANT_BOT_TOKEN as Text
, outputSize = 1000000
, interpretersSettings = interpreterSettings
} : Settings
