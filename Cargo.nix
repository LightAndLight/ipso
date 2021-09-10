# This file was @generated by cargo2nix 0.9.0.
# It is not intended to be manually edited.

args@{
  release ? true,
  rootFeatures ? [
    "ipso/default"
  ],
  rustPackages,
  buildRustPackages,
  hostPlatform,
  hostPlatformCpu ? null,
  hostPlatformFeatures ? [],
  mkRustCrate,
  rustLib,
  lib,
  workspaceSrc,
}:
let
  workspaceSrc = if args.workspaceSrc == null then ./. else args.workspaceSrc;
in let
  inherit (rustLib) fetchCratesIo fetchCrateLocal fetchCrateGit fetchCrateAlternativeRegistry expandFeatures decideProfile genDrvsByProfile;
  profilesByName = {
  };
  rootFeatures' = expandFeatures rootFeatures;
  overridableMkRustCrate = f:
    let
      drvs = genDrvsByProfile profilesByName ({ profile, profileName }: mkRustCrate ({ inherit release profile hostPlatformCpu hostPlatformFeatures; } // (f profileName)));
    in { compileMode ? null, profileName ? decideProfile compileMode release }:
      let drv = drvs.${profileName}; in if compileMode == null then drv else drv.override { inherit compileMode; };
in
{
  cargo2nixVersion = "0.9.0";
  workspace = {
    ipso = rustPackages.unknown.ipso."0.1.0";
  };
  "unknown".ipso."0.1.0" = overridableMkRustCrate (profileName: rec {
    name = "ipso";
    version = "0.1.0";
    registry = "unknown";
    src = fetchCrateLocal workspaceSrc;
    dependencies = {
      lazy_static = rustPackages."registry+https://github.com/rust-lang/crates.io-index".lazy_static."1.4.0" { inherit profileName; };
      paste = buildRustPackages."registry+https://github.com/rust-lang/crates.io-index".paste."1.0.5" { profileName = "__noProfile"; };
      typed_arena = rustPackages."registry+https://github.com/rust-lang/crates.io-index".typed-arena."2.0.1" { inherit profileName; };
    };
  });
  
  "registry+https://github.com/rust-lang/crates.io-index".lazy_static."1.4.0" = overridableMkRustCrate (profileName: rec {
    name = "lazy_static";
    version = "1.4.0";
    registry = "registry+https://github.com/rust-lang/crates.io-index";
    src = fetchCratesIo { inherit name version; sha256 = "e2abad23fbc42b3700f2f279844dc832adb2b2eb069b2df918f455c4e18cc646"; };
  });
  
  "registry+https://github.com/rust-lang/crates.io-index".paste."1.0.5" = overridableMkRustCrate (profileName: rec {
    name = "paste";
    version = "1.0.5";
    registry = "registry+https://github.com/rust-lang/crates.io-index";
    src = fetchCratesIo { inherit name version; sha256 = "acbf547ad0c65e31259204bd90935776d1c693cec2f4ff7abb7a1bbbd40dfe58"; };
  });
  
  "registry+https://github.com/rust-lang/crates.io-index".typed-arena."2.0.1" = overridableMkRustCrate (profileName: rec {
    name = "typed-arena";
    version = "2.0.1";
    registry = "registry+https://github.com/rust-lang/crates.io-index";
    src = fetchCratesIo { inherit name version; sha256 = "0685c84d5d54d1c26f7d3eb96cd41550adb97baed141a761cf335d3d33bcd0ae"; };
    features = builtins.concatLists [
      [ "default" ]
      [ "std" ]
    ];
  });
  
}
