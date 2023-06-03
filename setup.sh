apt update
apt upgrade
apt install leiningen
# https://serverfault.com/questions/112795/how-to-run-a-server-on-port-80-as-a-normal-user-on-linux
iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 3000
mkdir error_log
mkdir data
lein recache
lein jsbuild
