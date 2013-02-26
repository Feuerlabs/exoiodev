FeuerLabs io device demo
========================

Setup dm server
===============

Using scripts found in the exoiodev scripts directory you can 
create, delete and update various items.

Create the device type that is needed to execute bert rpc.

    ./create-device-type.sh io exodm_bert
    
Create a configuration set with spec and url
    
    ./create-config-set.sh ios exoio.yang https://localhost:8080

Provisioning 
============

    ./provision-bert-device.sh 1000 io ios 1 2

    ./provision-bert-device.sh 1001 io ios 1 2
    
    ./provision-bert-device.sh 1002 io ios 1 2
    
The code above create three devices 1000, 1001 and 1002 and
set them to the correct add them to the config-set ios
