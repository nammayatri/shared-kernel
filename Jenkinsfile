pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse "nammayatri"
            }
        }
        stage ('Nix Build All') {
            steps {
                nixBuildAll ()
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "nammayatri"
            }
        }
    }
}
