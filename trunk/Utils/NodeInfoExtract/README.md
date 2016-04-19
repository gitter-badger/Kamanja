The installer:

a) builds an Kamanja installation on the current machine
b) compresses and tars the installation  
c) invokes a scala script that extracts the node information from the supplied 
metadata store/configuration file.  
d) copies the tarball to each node/directory retrieved for each node
e) unpacks the tarball in place at each location

Notes:

a) The script is run by the user account that will have permissions on the target directories
on each machine to contain Kamanja nodes.
b) The user account must have been established on each machine.
c) That user account should be able to use a password-less ssh and scp to each machine to be 
included in the Kamanja cluster from every other machine to be part of the cluster. 
d) The build machine, upon which the install script is invoked, must also have this user account
and be able to use password-less ssh and scp.



