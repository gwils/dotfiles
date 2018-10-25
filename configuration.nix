# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  oldghcs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/83b35508c6491103cd16a796758e07417a28698b.tar.gz) {
    config = config // { allowBroken = true; };
  };
  unstable = import <nixpkgs-unstable> {};
  # newghcs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/f682ff93a2778f101d93b68c97278f902523758a.tar.gz) {
  newghcs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/7086e1fec3249397ae28c7b53d28c97c031ab6d9.tar.gz) {
    config = config // { allowBroken = true; };
  };

  trustee = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "gwils";
    repo = "trustee";
    rev = "nix-master";
    sha256 = "0plrji5kbqwajaajq4fppbv083bddacqip2vlz84jjlfm5glbg0k";
  }) {};

  hackage-cli = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "gwils";
    repo = "hackage-cli";
    rev = "nix";
    sha256 = "0zci86ywjprrhkf686frnwk8b9ffmq12x1g6w13gw5n39sv2nb2l";
  }) {};

in {

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.hostName = "gwilson"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  i18n.defaultLocale = "en_AU.UTF-8";

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    ag
    ant
    arandr
    aspell
    aspellDicts.en
    autoconf
    automake
    baobab
    bashCompletion
    bind
    binutils
    cabal2nix
    cairo
    chromium
    cmus
    cifs-utils
    clang
    cloc
    crawlTiles
    coreutils
    cowsay
    cups
    darcs
    deadbeef
    dmidecode
    dos2unix
    emacs
    evince
    exfat
    file
    fish
    flac
    gcc
    gimp
    ghostscript
    glibc
    glibc.dev
    gmp
    gmp.dev
    gnome3.eog
    gnome3.gedit
    gnome3.nautilus
    gnome3.zenity
    gnumake
    gnuplot
    # google-chrome
    graphviz
    hexedit
    htop
    # idea.idea-community
    inkscape
    imagemagick
    inetutils
    inotify-tools
    keepassx
    keepassx-community
    libedit
    libpng
    libtool
    libreoffice
    lolcat
    mcomix
    meld
    mitscheme
    mp3info
    mpv
    ncdu
    ncurses
    ncurses.dev
    octave
    oh-my-zsh
    openarena
    pandoc
    pcre
    pdfpc
    perl
    pinta
    pkgconfig
    pltScheme
    python
    python35
    python3
    python27Packages.pygments
    qpdf
    remmina
    sbt
    scala
    screenfetch
    shutter
    signal-desktop
    sl
    speedtest-cli
    spotify
    unstable.stack
    sublime
    terminator
    texlive.combined.scheme-full
    transmission
    tree
    unzip
    upower
    utillinux
    vim
    vlc
    wget
    which
    xclip
    xfce.thunar
    yubioath-desktop
    zsh

    notify-osd
    libnotify

    # nix-repl # superseded by nix repl
    nix-prefetch-scripts

    openconnect
    networkmanagerapplet
    networkmanager_openconnect

    xfontsel
    xlsfonts
    rxvt_unicode
    dmenu
    parcellite
    xsel
    xscreensaver
    haskellPackages.taffybar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    xorg.xbacklight

    gitAndTools.gitFull

    firefox
    thunderbird

    #pulseaudioFull
    pavucontrol

    zlib
    zlib.out
    pkgconfig

    unstable.haskellPackages.ghc
    unstable.haskellPackages.cabal-install
    haskellPackages.cabal-plan
    haskellPackages.alex
    haskellPackages.c2hs
    haskellPackages.cpphs
    #haskellPackages.djinn
    #haskellPackages.doctest
    unstable.haskellPackages.ghcid
    haskellPackages.hsc2hs
    haskellPackages.hpack
    haskellPackages.happy
    haskellPackages.hoogle
    haskellPackages.hscolour
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.packdeps

    oldghcs.haskell.compiler.ghc704
    oldghcs.haskell.compiler.ghc722
    oldghcs.haskell.compiler.ghc742
    oldghcs.haskell.compiler.ghc763
    oldghcs.haskell.compiler.ghc784
    haskell.compiler.ghc7103
    haskell.compiler.ghc802
    haskell.compiler.ghc822
    newghcs.haskell.compiler.ghc843
    unstable.haskell.compiler.ghc844
    newghcs.haskell.compiler.ghc861

    trustee
    hackage-cli

    #unstable.python36Packages.pip
    #unstable.python36Packages.pytest
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
      ubuntu_font_family
      source-code-pro
    ];
  };

  # List services that you want to enable:

  sound.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable upower service - used by taffybar's battery widget
  services.upower.enable = true;

  # Yubikey?
  services.pcscd.enable = true;

  hardware.pulseaudio.enable = true;


  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
  #networking.extraHosts = "192.168.1.188 trustee";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.startDbusSession = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";
  services.xserver.libinput.enable = true;
  services.xserver.resolutions = [ { x = 1920; y = 1080; } ];

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.default = "none";
  services.xserver.windowManager.default = "xmonad";
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages : [haskellPackages.taffybar];
  };

  programs = {
    java.enable = true;
    zsh = {
      enable = true;
      shellAliases = {};
      enableCompletion = true;
      ohMyZsh.enable = true;
    };
  };

  ### Virtualisation
  virtualisation.virtualbox.host.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest // {
    virtualbox = pkgs.linuxPackages.virtualbox.override {
      # enableExtensionPack = true;
      pulseSupport = true;
    };
  };
  #nixpkgs.config.virtualbox.enableExtensionPack = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.gwilson = {
    name = "gwilson";
    createHome = true;
    description = "George Wilson";
    extraGroups = [ "wheel" "networkmanager" "virtualbox" "vboxusers"];
    home = "/home/gwilson";
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";

  nix.binaryCaches = [
    "https://cache.nixos.org"
    "https://hydra.qfpl.io"
    "https://mpickering.cachix.org"
    "https://nixcache.reflex-frp.org"
  ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "qfpl.io:xME0cdnyFcOlMD1nwmn6VrkkGgDNLLpMXoMYl58bz5g="
    "mpickering.cachix.org-1:COxPsDJqqrggZgvKG6JeH9baHPue8/pcpYkmcBPUbeg="
  ];

}

