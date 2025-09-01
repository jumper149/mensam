FROM mcr.microsoft.com/devcontainers/base:alpine

# Install nix
RUN curl --output nix-installer.sh --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install
RUN chmod +x nix-installer.sh
RUN ./nix-installer.sh --daemon --yes
RUN echo 'accept-flake-config = true' >> /etc/nix/nix.conf
RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
RUN echo 'substituters = https://cache.nixos.org/ https://jumper149-mensam.cachix.org' >> /etc/nix/nix.conf
RUN echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= jumper149-mensam.cachix.org-1:9502wAOm00GdLxZM8uTE4goaBGCpHb+d1jUt3dhR8ZM=' >> /etc/nix/nix.conf

# Install direnv
RUN apk add direnv
RUN mkdir -p /root/.config/direnv/
RUN echo $'[whitelist]\n\
  prefix = [ "/" ]\n\
' > /root/.config/direnv/direnv.toml
RUN git clone https://github.com/nix-community/nix-direnv.git /usr/share/nix-direnv-source
RUN echo 'source /usr/share/nix-direnv-source/direnvrc' > /root/.config/direnv/direnvrc

# Choose bash as the default shell
RUN chsh --shell bash root

# Run the Nix daemon in the background
ENTRYPOINT ["/nix/var/nix/profiles/default/bin/nix-daemon"]
CMD ["--daemon"]
