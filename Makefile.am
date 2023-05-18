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
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

NOCOMP_SOURCES = asahi/guix/packages/defconfig.main \
		 asahi/guix/packages/defconfig.edge \
		 asahi/guix/substitutes.asahi-guix.org.pub

SOURCES = asahi/guix/bootloader/m1n1.scm \
          asahi/guix/packages/linux.scm \
          asahi/guix/packages/crates-io.scm \
          asahi/guix/packages/gl.scm \
          asahi/guix/packages/audio.scm \
          asahi/guix/packages/firmware.scm \
          asahi/guix/packages/rust.scm \
          asahi/guix/packages/bootloader.scm \
          asahi/guix/packages/jemalloc.scm \
          asahi/guix/packages/misc.scm \
          asahi/guix/build/bootloader/m1n1.scm \
          asahi/guix/build/block-devices.scm \
          asahi/guix/build/firmware.scm \
          asahi/guix/system/desktop.scm \
          asahi/guix/system/base.scm \
          asahi/guix/system/install.scm \
          asahi/guix/services/channels.scm \
          asahi/guix/services/firmware.scm \
          asahi/guix/services/udev.scm \
          asahi/guix/services/console-font.scm \
          asahi/guix/channels.scm \
          asahi/guix/initrd.scm \
          asahi/guix/udev.scm \
          asahi/guix/transformations.scm \
          asahi/guix/substitutes.scm

TESTS = tests/asahi/guix/build/block-devices.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
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
