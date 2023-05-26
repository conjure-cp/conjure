class Conjure < Formula
  desc "Conjure: The Automated Constraint Modelling Tool"
  homepage "https://github.com/conjure-cp/conjure"
  url "https://github.com/conjure-cp/conjure/archive/refs/tags/v2.4.0.tar.gz"
  sha256 "2c5aa0065d00d8289b2da93a5d6dbe276d7248c6dd63de09096741ac89c4dc1d"
  license "BSD-3-Clause"

  # depends_on "ghc@8.6" => :build # rely on conjure to sort this out
  depends_on "python@3.11" => :build # for docs
  depends_on "openjdk" # for Savile Row
  # depends_on "z3"

  def install
    # pick up dependencies to build the documentation
    system "pip3", "install", "sphinx-rtd-theme", "sphinxcontrib-bibtex"

    # place binaries where brew wants to put them
    ENV["BIN_DIR"] = "#{bin}"
    system "make", "install"
    # to build kissat a C compiler must be installed, e.g. clang from Xcode
    # system "etc/build/install-kissat.sh"

    system "make", "docs"
    doc.install "docs/_build/latex/Conjure.pdf"
    doc.install Dir["docs/_build/singlehtml/*"]
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    # Run the test with `brew test conjure`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "cp", "#{buildpath}/docs/tutorials/futoshiki/futoshiki.essence", "."
    system "cp", "#{buildpath}/docs/tutorials/futoshiki/futoshiki.essence-param", "."
    assert_match "Copying solution to:", shell_output(
      "#{bin}/conjure solve futoshiki.essence futoshiki.essence-param"
    ).chomp
  end

end
