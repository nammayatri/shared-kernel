pipeline {
    agent none
    options {
        parallelsAlwaysFailFast()
    }
    stages {
        stage ('Matrix') {
            matrix {
                agent {
                    label "${SYSTEM}"
                }
                axes {
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux', 'aarch64-darwin', 'x86_64-darwin'
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse "nammayatri"
                        }
                    }
                    stage ('Nix Build All') {
                        steps {
                            nixCI system: env.SYSTEM
                        }
                    }
                    stage ('Cachix push') {
                        when {
                            anyOf {
                                branch 'main'; branch 'prodHotPush'
                            }
                        }
                        steps {
                            cachixPush "nammayatri"
                        }
                    }
                }
            }
        }
    }
}
