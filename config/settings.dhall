let nixProfile = env:NIX_PROFILE as Text
let leanBinPath = env:LEAN_BIN_PATH as Text
let lakeBinPath = env:LAKE_BIN_PATH as Text
let leanProjectPath = env:LEAN_PROJECT_PATH as Text

let Limit = { soft : Natural, hard : Natural }
let SandboxSettings = { sandboxExecutable : Text, sandboxArgs : List Text }
let ExternalSettings =
      { args : List Text
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
      , sandbox : Optional SandboxSettings
      }
let InternalSettings =
      { timeout : Natural
      , allocations : Natural
      , inputSize : Natural
      , sourceFilePrefix : Text
      , sourceFileExtension : Text
      }
let AgdaSettings = { internal : InternalSettings }
let LeanSettings =
      { projectDir : Text
      , externalLean : ExternalSettings
      }
let ArendSettings =
      { arendRootProjectDir : Text
      , arendYamlFilename : Text
      , arendYamlContent : Text
      , arendModuleName : Text
      , externalArend : ExternalSettings
      }
let AlloySettings =
      { alloyProjectDir : Text
      , alloySharedDir : Text
      , dotGraphExecutable : Text
      , dotGraphArgs : List Text
      , imageHelperExecutable : Text
      , imageHelperArgs : List Text
      , gifConverterExecutable : Text
      , gifConverterArgs : List Text
      , externalAlloy : ExternalSettings
      }
let defaultResource =
      { totalMemory = { soft = 5, hard = 10 }
      , dataSize = { soft = 2176782336, hard = 2176782336 }
      , openFiles = { soft = 7, hard = 7 }
      , fileSize = { soft = 10800, hard = 10800 }
      , cpuTime = { soft = 4, hard = 5 }
      }
let emptyExternalSettings =
      { args = [] : List Text
      , executable = ""
      , time = 5
      , priority = 20
      , resources = defaultResource
      , fileExtension = ""
      , inputSize = 1000000
      , tempFilePrefix = ""
      , sandbox = None SandboxSettings
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
let leanSettings =
      { externalLean = emptyExternalSettings //
          { args = ["env", leanBinPath, "--profile", "--run"] : List Text
          , executable = lakeBinPath
          , tempFilePrefix = "lean"
          , fileExtension = "lean"
          , time = 10
          , sandbox = Some
              { sandboxExecutable = "/usr/bin/bwrap"
              , sandboxArgs =
                [ "--unshare-all"
                -- environmental variables
                , "--setenv", "LAKE_BIN_PATH", lakeBinPath
                , "--setenv", "LEAN_BIN_PATH", leanBinPath
                , "--setenv", "LEAN_PROJECT_PATH", leanProjectPath
                -- directories binds
                , "--ro-bind", "/lib", "/lib"
                , "--ro-bind", "/lib64", "/lib64"
                , "--ro-bind", "/nix/store", "/nix/store"
                , "--ro-bind", leanProjectPath, leanProjectPath
                , "--ro-bind", nixProfile, nixProfile
                -- runtime
                , "--proc", "/proc"
                , "--dev", "/dev"
                -- current directory
                , "--chdir", leanProjectPath
                ] : List Text
            }
          }
      , projectDir = leanProjectPath
      }
let agdaSettings =
      { internal = emptyInternalSettings //
          { sourceFilePrefix = "agda"
          , sourceFileExtension = "agda"
          , timeout = 60
          }
      }
let rzkSettings = emptyInternalSettings // { timeout = 60 }
let _arendLibDir = env:AREND_STDLIB_PATH as Text
let _arendRootProjectDir = env:AREND_ROOT_PROJECT_DIR as Text
let _arendJar = env:AREND_PATH as Text
let _javaHome = env:JAVA_HOME as Text
let arendSettings = { arendRootProjectDir = _arendRootProjectDir
                    , arendYamlFilename = "arend.yaml"
                    , arendYamlContent = "dependencies: [arend-lib]"
                    , arendModuleName = "Main"
                    , externalArend = emptyExternalSettings //
                        { args = ["-jar", "${_arendJar}", "-larend-lib", "-L${_arendLibDir}/libs"]
                        , executable = "${_javaHome}/bin/java"
                        , tempFilePrefix = "arend"
                        , fileExtension = "ard"
                        , time = 60
                        }
                    }
let _alloyProjectDir = env:ALLOY_PROJECT_DIR as Text
let _alloySharedDir = env:PROOF_ASSISTANT_SHARED_IMG_DIR as Text
let _alloyJar = env:ALLOY_PATH as Text
let alloySettings =
      { alloyProjectDir = _alloyProjectDir
      , dotGraphExecutable = "dot"
      , dotGraphArgs =
          [ "-Tpng:cairo"
          ]
      , imageHelperExecutable = "identify"
      , imageHelperArgs =
          [ "-format", "%[fx:w]-%[fx:h]\n" ]
          
      , gifConverterExecutable = "convert"
      , gifConverterArgs =
          [ "-delay", "80x100"
          , "-deconstruct"
          ]
      , alloySharedDir = _alloySharedDir
      , externalAlloy = emptyExternalSettings //
          { executable = "${_javaHome}/bin/java"
          , args =
              [ "-classpath"
              , "${_alloyJar}:${_alloyProjectDir}/bin"
              , "Main"
              ]
          , tempFilePrefix = "alloy"
          , fileExtension = "als"
          , time = 60
          }
      }
let Settings = { botName : Text
               , botToken : Text
               , interpretersSettings : { agda : AgdaSettings
                                        , arend : ArendSettings
                                        , idris : ExternalSettings 
                                        , coq : ExternalSettings 
                                        , lean : LeanSettings 
                                        , rzk : InternalSettings
                                        , alloy : AlloySettings
                                        }
               , outputSize : Natural
               , help : Text
               , version : Text
               , helpMessages : List { mapKey : Text, mapValue : Text }
               , sharedDir : Text
               }
let interpreterSettings =
      { agda = agdaSettings          
      , arend = arendSettings
      , idris = idrisSettings
      , coq = coqSettings
      , lean = leanSettings
      , rzk  = rzkSettings
      , alloy = alloySettings
      }

in

{ botName = "ProofAssistantBot"
, botToken = env:PROOF_ASSISTANT_BOT_TOKEN as Text
, outputSize = 1000000
, sharedDir = _alloySharedDir
, help =
    ''
    Proof Assistant Help:

    /help - display this message.

    /coq <input> - typecheck your input.

    /agda /load <input> - load and typecheck your input into Agda state.

    /agda /reload - reload previously loaded input.

    /agda /typeOf <expr> - get type of the given expression.

    /agda <expr> - evaluate the given expression.

    /idris2 /load <input> - load and typecheck your input. return `the core` of program.

    /idris2 /typeOf <expr> - typecheck the given expression.

    /idris2 <expr> - also typecheck the given expression.

    /lean <input> - typecheck your input.

    /arend <input> - typecheck your input.

    /rzk #lang - typecheck input in the given language. See list of all supported languages here: github:fizruk/rzk.

    /alloy <input> - typecheck your input. Should return either text, PNG or GIF.
    ''
, helpMessages = [] : List { mapKey : Text, mapValue : Text }
, version =
  ''
  Proof Assistant Bot v.<bot_version>
  https://github.com/swamp-agr/proof-assistant-bot
  
  Agda (lib) <agda_version>

  The Coq Proof Assistant, version 8.16.1
  compiled with OCaml 4.14.0

  Idris 2, version 0.6.0

  Lean (version 3.49.0, commit acf633e01a87, Release)

  Arend 1.9

  Alloy 6.1.0.202111031525
  ''
, interpretersSettings = interpreterSettings
} : Settings
