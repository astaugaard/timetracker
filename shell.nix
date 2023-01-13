{ pkgs ? import <nixpkgs> {} }:
    pkgs.mkShell {
        nativeBuildInputs = [ pkgs.postgresql
                            ];
        shellHook = ''
            mkdir .nix-shell
            export NIX_SHELL_DIR=$PWD/.nix-shell
            export PGDATA=$NIX_SHELL_DIR/db
            trap \
                "
                  echo closing db!!!
                  pg_ctl -D $PGDATA stop
                  cd $PWD
                  rm -rf $NIX_SHELL_DIR
                " \
                EXIT
            if ! test -d $PGDATA
            then
                pg_ctl initdb -D $PGDATA
            fi
            pg_ctl                                                  \
                -D $PGDATA                                          \
                -l $PGDATA/postgres.log                             \
                -o "-c unix_socket_directories='$PGDATA'"           \
                -o "-c listen_addresses='*'"                        \
                -o "-c log_destination='stderr'"                    \
                -o "-c logging_collector=on"                        \
                -o "-c log_directory='log'"                         \
                -o "-c log_filename='postgreql-%Y-%m-%d_%H%M%S.log'"\
                -o "-c log_min_messages=info"                       \
                -o "-c log_min_error_statement=info"                \
                -o "-c log_connections=on"                          \
                start
        '';
}
