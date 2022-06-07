This is our Firesim configuration. 

First you need to download the firesim project, git clone https://github.com/firesim/firesim.git.

Then replace the four basic configurations in the firesim/deploy directory with the following four files, config_build.ini, config_build_recipes.ini, config_hwdb.ini, config_runtime.ini.

Use the FireMarshal tool to generate the spec6-int.img and spec6-int-bin files based on the Firesim-deploy/FireMarshal/spec6-int.json file.

Then copy all the files in the Firesim-deploy/workloads directory to the firesim/deploy/workloads directory, run
"./run-workloads.sh spec6.int.ini --withlaunch" can start firesim automatically.

Then use ssh to log in to the linux simulated by firesim, account root, and password is firesim. The rest of the steps are the same as described in the firesim documentation: https://docs.fires.im/en/stable/Running-Simulations-Tutorial/index.html
