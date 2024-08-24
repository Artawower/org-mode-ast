((nil . ((eval . (add-to-list 'dape-configs
             `(jest-table
               modes (js-mode js-ts-mode typescript-ts-mode)
               command "node"
               command-cwd "/Users/darkawower/tmp/vscode-js-debug"
               command-args ("src/dapDebugServer.js" "8123")
               host "localhost" ;; Missing from your config
               port 8123 ;; Missing from your config
               :type "pwa-node"
               :request "launch"
               :cwd dape-cwd-fn
               :program "/Users/darkawower/projects/pet/org-mode-ast/node_modules/.bin/jest"
               :outputCapture "console"
               :args [ "--config"
                       "/Users/darkawower/projects/pet/org-mode-ast/jest.config.cjs"
                       "-i"
                       "/Users/darkawower/projects/pet/org-mode-ast/src/parser/handlers/table.spec.ts"
                       "--no-watch"]
               :runtimeExecutable nil
               :sourceMaps t
               :pauseForSourceMap nil
               :enableContentValidation t
               :disableOptimisticBPs: t
               :resolveSourceMapLocations [
                                            ,(concat (dape--default-cwd) "src/**")
                                            "!**/node_modules/**"
                                            ]
               :autoAttachChildProcesses t
               :console "internalConsole"
               :killBehavior "forceful"
               ))
         
))))
