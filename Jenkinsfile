pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'nix run nixpkgs#cachix use nammayatri'
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
        /* stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run .#cachix-push'
          }
        } */
    }
}
