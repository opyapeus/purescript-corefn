let conf = ./spago.dhall

in conf // {
  sources =  [ "src/**/*.purs", "testself/**/*.purs" ],
  dependencies = conf.dependencies # [ "node-fs", "test-unit" ]
}
