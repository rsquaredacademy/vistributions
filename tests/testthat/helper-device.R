# Keep the test suite from writing a graphics file into the source tree.
#
# A test that prints a plot without opening its own device makes R open the
# default device, which on Windows and macOS writes `Rplots.pdf` in the working
# directory (tests/testthat, which was committed by accident). Opening a pdf
# device whose file lives in the session temporary directory here means any
# such plot is written there instead, and the source tree stays clean.
grDevices::pdf(file.path(tempdir(), "vistributions-test-plots.pdf"))
