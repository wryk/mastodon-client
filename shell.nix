let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
  }) {};

  # 2021-04-19 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "d56c436a66ec2a8a93b309c83693cef1507dca7a";
    sha256 = "1jhmfw1d4gbh9qi92x6ka9iy6kp5mjyzrby9xvs4603njlcv3pjg";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "mastodon-client";
  buildInputs = with pursPkgs; [
    pursPkgs.purs-0_13_2
    pursPkgs.spago
    pkgs.nodejs-14_x
  ];
}
