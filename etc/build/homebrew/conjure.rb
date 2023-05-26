cask "conjure" do

  arch arm: "arm", intel: "intel"

  version "2.4.0"
  sha256 arm:   "0634a83c895abee4124236ea4ff297393bc1f1756461eeaa766815d6be5a5bc8",
         intel: "3e2e3d7a56181c6a12785a9081cde789cad4a91480d8a3de78c5d6d0f85fc516"

  url "https://github.com/conjure-cp/conjure/releases/download/v#{version}/conjure-v#{version}-macos-#{arch}-with-solvers.zip",
      verified: "github.com/conjure-cp/conjure"
  name "conjure"
  desc "Conjure: The Automated Constraint Modelling Tool"
  homepage "https://conjure.readthedocs.io/en/latest/welcome.html"

  # fix: unclear how to handle JDK dependency
  # depends_on cask:"openjdk"
  # fix: use brew z3 if installed, otherwise install ours
  # depends_on cask:"z3"

  binary "conjure-v#{version}-macos-#{arch}-with-solvers/bc_minisat_all_release"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/boolector"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/cadical"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/conjure"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/fzn-chuffed"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/fzn-gecode"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/glucose"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/glucose-syrup"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/kissat"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/lingeling"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/minion"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/nbc_minisat_all_release"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/open-wbo"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/plingeling"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/savilerow"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/savilerow.jar"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/treengeling"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/yices"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/yices-sat"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/yices-smt"
  binary "conjure-v#{version}-macos-#{arch}-with-solvers/yices-smt2"
# conflicts with already installed brew z3:
#  binary "conjure-v#{version}-macos-#{arch}-with-solvers/z3"

end
