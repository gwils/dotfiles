# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let antigen =
  pkgs.fetchgit {
    url = "https://github.com/zsh-users/antigen";
    rev = "1d212d149d039cc8d7fdf90c016be6596b0d2f6b";
    sha256 = "1c7ipgs8gvxng3638bipcj8c1s1dn3bb97j8c73piv2xgk42aqb9";
  };
in {

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
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

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    ag
    arandr
    aspell
    aspellDicts.en
    bashCompletion
    binutils
    cabal2nix
    deadbeef
    google-chrome
    cifs-utils
    cloc
    coreutils
    exfat
    fish
    gnome3.nautilus
    htop
    ncdu
    oh-my-zsh
    pandoc
    remmina
    sublime
    terminator
    tree
    unzip
    upower
    utillinux
    vim
    wget
    which
    xclip
    xfce.thunar
    zsh

    notify-osd
    libnotify

    nix-repl
    nix-prefetch-scripts

    openconnect
    networkmanagerapplet

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

    gitAndTools.gitFull

    firefox
    thunderbird

    pulseaudioFull
    pavucontrol

    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.alex
    haskellPackages.djinn
    haskellPackages.happy
    haskellPackages.hoogle
    haskellPackages.cpphs
    haskellPackages.doctest
    haskellPackages.hscolour
    haskellPackages.ghcid
    haskellPackages.hindent
    haskellPackages.hlint
  ];

  fileSystems."/mnt/machines" = {
    device = "//192.168.1.4/machines";
    fsType = "cifs";
    options = [ "username=machines" "password=machines" "x-systemd.automount" "noauto" ];
  };

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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable upower service - used by taffybar's battery widget
  services.upower.enable = true;

  hardware.pulseaudio.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
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

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.gwilson = {
    name = "gwilson";
    createHome = true;
    description = "George Wilson";
    extraGroups = [ "wheel" "networkmanager"];
    home = "/home/gwilson";
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  nix.binaryCaches = [
    "https://nixcache.reflex-frp.org"
  ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
  ];

}

