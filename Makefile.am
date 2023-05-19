bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -L src -o "$@" "$<"

NOCOMP_SOURCES = src/asahi/guix/packages/defconfig.main \
		 src/asahi/guix/packages/defconfig.edge \
		 src/asahi/guix/substitutes.asahi-guix.org.pub

SOURCES = src/asahi/guix/bootloader/m1n1.scm \
          src/asahi/guix/packages/linux.scm \
          src/asahi/guix/packages/crates-io.scm \
          src/asahi/guix/packages/gl.scm \
          src/asahi/guix/packages/audio.scm \
          src/asahi/guix/packages/firmware.scm \
          src/asahi/guix/packages/rust.scm \
          src/asahi/guix/packages/bootloader.scm \
          src/asahi/guix/packages/misc.scm \
          src/asahi/guix/build/bootloader/m1n1.scm \
          src/asahi/guix/build/block-devices.scm \
          src/asahi/guix/build/firmware.scm \
          src/asahi/guix/system/desktop.scm \
          src/asahi/guix/system/base.scm \
          src/asahi/guix/system/install.scm \
          src/asahi/guix/services/channels.scm \
          src/asahi/guix/services/firmware.scm \
          src/asahi/guix/services/udev.scm \
          src/asahi/guix/services/console-font.scm \
          src/asahi/guix/channels.scm \
          src/asahi/guix/initrd.scm \
          src/asahi/guix/udev.scm \
          src/asahi/guix/transformations.scm \
          src/asahi/guix/substitutes.scm

TESTS = tests/asahi/guix/build/block-devices.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main -L src     \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/ashai-guix.texi
dvi: # Don't build dvi docs

EXTRA_DIST += NOTES.org \
              README.org \
              README \
              COPYING \
              HACKING \
              .guix-authorizations \
              .guix-channel \
              .gitignore \
              manifest.scm \
              share/systems/asahi-guix.tmpl \
              share/systems/asahi-guix-edge.tmpl \
              .github/workflows/aarch64-linux-gnu.yml \
              .github/workflows/x86_64-linux-gnu.yml \
              .github/FUNDING.yml \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
