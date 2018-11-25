This will allow eshell to autocomplete SSH hosts from the list of
known hosts in your ~/.ssh/known_hosts file. Note that newer
versions of ssh hash the hosts by default to prevent Island-hopping
SSH attacks. (https://itso.iu.edu/Hashing_the_OpenSSH_known__hosts_File)
You can disable this by putting the following line in your ~/.ssh/config
file following the "Host *" directive:

HashKnownHosts no

Note that this will make you vulnerable to the Island-hopping
attack described in the link above if you allow key-based
passwordless logins and your account is compromised.
