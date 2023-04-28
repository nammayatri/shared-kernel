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
            when {
                anyOf { branch 'main'; branch 'prodHotPush' }
            }
            steps {
                cachixPush "nammayatri"
            }
        }
    }
}
