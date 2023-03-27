pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse 'nammayatri'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build -L'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build -L .#check'
            }
        }
        stage ('Cachix push') {
            steps {
              cachixPush "nammayatri"
            }
        }
    }
}
