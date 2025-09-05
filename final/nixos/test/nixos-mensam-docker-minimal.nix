moduleMensam: { lib, pkgs, ... }: {
  imports = [ moduleMensam ];
  virtualisation.diskSize = 8192;
  virtualisation.memorySize = 2048;
  virtualisation.oci-containers.containers.mensam.image = lib.mkForce "docker-archive://${pkgs.mensam.dockerImage}";
  services.mensam = {
    enable = true;
    provider = "docker";
  };
}
