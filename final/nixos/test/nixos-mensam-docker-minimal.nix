moduleMensam: { lib, pkgs, ... }: {
  imports = [ moduleMensam ];
  virtualisation.diskSize = 8192;
  virtualisation.memorySize = 2048;
  services.mensam = {
    enable = true;
    provider = "docker";
    dockerImage = pkgs.mensam.dockerImage;
  };
}
