#!/bin/bash

# Guix on Hetzner

# Test on:
# - Ubuntu 21.04
# - Debian 11
# - Debian 9 (see comments ~ line 48)
#
# 1. Create a new CCX11 with Debian 11
# 2. Login with SSH
# 3. Paste this script into a setup.sh and run it
# 4. Wait ~10 minutes on a 25 Euro Cloud Server
#
# If you just want to deploy to the target machine, from your PantherX installation, this is all you need to do.
# If you want to operate on the target machine directly, you need to run the 2nd script pantherx-on-hetzner-cloud_second.sh
#
# Modify below values to suit your needs. Change the password!

###### MODIFY

TIMEZONE="Europe/Berlin"
LOCALE="en_US.utf8"
USERNAME="panther"
USER_COMMENT="panther's account"
USER_PASSWORD="6a4NQqrp84Y7mj56"

HOSTNAME='asahi-guix'

###### MODIFY END

if [ "$USER_PASSWORD" == "6a4NQqrp84Y7mj56" ]; then
	echo "######### IMPORTANT #########"
	echo "You did not modify the default user password. Overwriting ..."
	USER_PASSWORD=$(openssl rand -base64 24)
	echo
	echo "#########"
	echo $USER_PASSWORD
	echo "#########"
	echo
fi

CONFIG=/etc/bootstrap-config.scm
CHANNELS=/etc/channels.scm
CRYPT='$6$abc'

apt-get update -y
apt-get install curl xz-utils -y
# Uncomment this for Debian 9
# sed -i '/^mozilla\/DST_Root_CA_X3/s/^/!/' /etc/ca-certificates.conf && update-ca-certificates -f
# wget https://ftpmirror.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz
wget https://ftpmirror.gnu.org/gnu/guix/guix-binary-1.4.0.aarch64-linux.tar.xz
cd /tmp
tar --warning=no-timestamp -xf ~/guix-binary-1.4.0.aarch64-linux.tar.xz
mv var/guix /var/ && mv gnu /
mkdir -p ~root/.config/guix
ln -sf /var/guix/profiles/per-user/root/current-guix ~root/.config/guix/current
export GUIX_PROFILE="`echo ~root`/.config/guix/current" ;
source $GUIX_PROFILE/etc/profile
groupadd --system guixbuild
for i in `seq -w 1 10`;
do
   useradd -g guixbuild -G guixbuild         \
           -d /var/empty -s `which nologin`  \
           -c "Guix build user $i" --system  \
           guixbuilder$i;
done;

cp ~root/.config/guix/current/lib/systemd/system/guix-daemon.service /etc/systemd/system/
systemctl start guix-daemon && systemctl enable guix-daemon
mkdir -p /usr/local/bin
cd /usr/local/bin
ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix
mkdir -p /usr/local/share/info
cd /usr/local/share/info
for i in /var/guix/profiles/per-user/root/current-guix/share/info/*; do
    ln -s $i; done
guix archive --authorize < ~root/.config/guix/current/share/guix/ci.guix.gnu.org.pub
guix pull
guix package -i glibc-utf8-locales
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
guix package -i openssl

function write_server_config() {
cat >> $CONFIG <<EOL
;; Server Configuration (plain) v1
;; /etc/system.scm

(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules screen ssh certs tls)

(operating-system
  (host-name "${HOSTNAME}")
  (timezone "${TIMEZONE}")
  (locale "${LOCALE}")

  (initrd-modules (append (list "virtio_scsi")
                                %base-initrd-modules))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))

  (file-systems (append
        (list (file-system
                (device "/dev/sda1")
                (mount-point "/")
                (type "ext4")))
              %base-file-systems))

  (users (cons (user-account
                (name "${USERNAME}")
                (comment "${USER_COMMENT}")
                (group "users")
		        (password (crypt "${USER_PASSWORD}" "${CRYPT}"))

                (supplementary-groups '("wheel"))
                (home-directory "/home/${USERNAME}"))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (cons* screen openssh nss-certs gnutls %base-packages))

  (services (cons*
  (service dhcp-client-service-type)
  (service openssh-service-type
  		  (openssh-configuration
		  (permit-root-login 'without-password)))
  %base-services)))
EOL
}

function write_system_channels() {
cat >> $CHANNELS <<EOL
;; Default Channels

(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (branch "master")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
EOL
}

write_server_config
write_system_channels
#write_signing_key
# guix archive --authorize < /etc/packages.pantherx.org.pub
# guix pull --disable-authentication --channels=/etc/channels.scm
# hash guix

guix system build /etc/bootstrap-config.scm
# these appear to be the necessary on Ubuntu 21.04
mv /etc/ssl /etc/bk_ssl
mv /etc/pam.d /etc/bk_pam.d
mv /etc/skel /etc/bk_skel

guix system reconfigure /etc/bootstrap-config.scm
mv /etc /old-etc
mkdir /etc
cp /old-etc/resolv.conf /etc/resolv.conf
cp -r /old-etc/resolvconf /etc/resolvconf
cp -r /old-etc/{passwd,group,shadow,gshadow,mtab,guix,bootstrap-config.scm} /etc/
cp /old-etc/channels.scm /etc/guix/channels.scm
guix system reconfigure /etc/bootstrap-config.scm

echo "We are done here."
echo $HOSTNAME

# reboot
