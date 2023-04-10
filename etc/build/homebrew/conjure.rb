cask "conjure" do

  arch arm: "arm", intel: "intel"

  version "2.4.0"
  sha256 arm:   "0634a83c895abee4124236ea4ff297393bc1f1756461eeaa766815d6be5a5bc8",
         intel: "???"

  url "https://github.com/conjure-cp/conjure/releases/download/v#{version}/conjure-v#{version}-macos-#{arch}-with-solvers.zip",
      verified: "github.com/conjure-cp/conjure"
  name "conjure"
  desc "Conjure: The Automated Constraint Modelling Tool"
  homepage "https://conjure.readthedocs.io/en/latest/welcome.html"

  binary "conjure-v#{version}-macos-arm-with-solvers/bc_minisat_all_release"
  binary "conjure-v#{version}-macos-arm-with-solvers/boolector"
  binary "conjure-v#{version}-macos-arm-with-solvers/cadical"
  binary "conjure-v#{version}-macos-arm-with-solvers/conjure"
  binary "conjure-v#{version}-macos-arm-with-solvers/fzn-chuffed"
  binary "conjure-v#{version}-macos-arm-with-solvers/fzn-gecode"
  binary "conjure-v#{version}-macos-arm-with-solvers/glucose"
  binary "conjure-v#{version}-macos-arm-with-solvers/glucose-syrup"
  binary "conjure-v#{version}-macos-arm-with-solvers/kissat"
  binary "conjure-v#{version}-macos-arm-with-solvers/lingeling"
  binary "conjure-v#{version}-macos-arm-with-solvers/minion"
  binary "conjure-v#{version}-macos-arm-with-solvers/nbc_minisat_all_release"
  binary "conjure-v#{version}-macos-arm-with-solvers/open-wbo"
  binary "conjure-v#{version}-macos-arm-with-solvers/plingeling"
  binary "conjure-v#{version}-macos-arm-with-solvers/savilerow"
  binary "conjure-v#{version}-macos-arm-with-solvers/savilerow.jar"
  binary "conjure-v#{version}-macos-arm-with-solvers/treengeling"
  binary "conjure-v#{version}-macos-arm-with-solvers/yices"
  binary "conjure-v#{version}-macos-arm-with-solvers/yices-sat"
  binary "conjure-v#{version}-macos-arm-with-solvers/yices-smt"
  binary "conjure-v#{version}-macos-arm-with-solvers/yices-smt2"
  binary "conjure-v#{version}-macos-arm-with-solvers/z3"

end
