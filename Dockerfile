FROM postgres:15

ENV PATH $PATH:/tmp/bin
ENV ADA_PROJECT_PATH /tmp/ahven-2.8/gnat
ENV LD_LIBRARY_PATH /tmp/mimsy/alire/cache/pins/jwt/.objs/.libs:/tmp/matreshka/.libs:/tmp/mimsy/alire/cache/dependencies/aws_21.0.0_57fddf8f/install_dir/lib

RUN <<EOF
  set -ex
  apt update
  apt install -y --no-install-recommends git curl wget gnat gprbuild jq make \
   ca-certificates unzip libpq-dev gcc inotify-tools
  rm -rf /var/lib/apt/lists/*
EOF

RUN su postgres <<EOF
set -ex
   cd /tmp/
   git clone --depth=1 https://github.com/reznikmm/mimsy
   mkdir -p -v "/var/lib/postgresql/.config/Matreshka Project/"
   cp -v mimsy/share/mimsy/*.conf "/var/lib/postgresql/.config/Matreshka Project/"
   curl https://www.ahven-framework.com/releases/ahven-2.8.tar.gz | tar xzf -
   git clone --depth=1 https://github.com/godunko/matreshka
   sed -i -e /gnatcoll/d matreshka/gnat/matreshka_spikedog_aws.gpr
   curl -L -O https://github.com/alire-project/alire/releases/download/v1.2.2/alr-1.2.2-bin-x86_64-linux.zip
   unzip alr-1.2.2-bin-x86_64-linux.zip
   alr toolchain --select gnat_external
   cd mimsy
   alr build
   make install
   alr exec -- gprbuild -j0 -p -P ../matreshka/gnat/matreshka_spikedog_awsd.gpr
   gprbuild -p -P ahven.gpr
EOF

ENV POSTGRES_PASSWORD pgsqlsecret

RUN <<EOF
  set -ex
  /usr/local/bin/docker-entrypoint.sh postgres > /tmp/psql.log 2>&1 &
  sleep 8
  su postgres -c "createdb mail" || cat /tmp/psql.log
  su postgres -c "psql -d mail -f /tmp/mimsy/source/sql/create.sql"

  cat <<DONE > /usr/local/bin/mimsy.sh
#!/bin/bash
echo Open http://localhost:3333/game/index.html
/usr/local/bin/docker-entrypoint.sh postgres > /tmp/psql.log 2>&1 &
cd /tmp/mimsy/
su postgres -c share/mimsy/monitor.sh > /tmp/monitor.log 2>&1 &
su postgres -c /tmp/matreshka/.objs/spikedog_awsd/spikedog_awsd
DONE

  chmod +x /usr/local/bin/mimsy.sh
EOF

ENTRYPOINT ["mimsy.sh"]

EXPOSE 3333
