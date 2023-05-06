import vdf
import json
import subprocess
import os
import sys

class SteamStuff:
	said0 = subprocess.run(["emacsclient", "--eval", '(format \"%s\" steam_account_id)'], stdout=subprocess.PIPE)
	said1 = said0.stdout.decode('utf-8').rstrip().replace('"', '')
	useremacsdir = subprocess.run(["emacsclient", "--eval", '(format \"%s\" user-emacs-directory)'], stdout=subprocess.PIPE)
	useremacsdir = useremacsdir.stdout.decode('utf-8').rstrip().replace('"', '').replace('~', os.getenv('HOME'))
	steamacsdir = subprocess.run(["emacsclient", "--eval", '(format \"%s\" steamdir)'], stdout=subprocess.PIPE)
	steamacsdir = useremacsdir + steamacsdir.stdout.decode('utf-8').rstrip().replace('"', '')
	localconfig = os.getenv("HOME") + "/.steam/steam/userdata/" + said1 + "/config/localconfig.vdf"
	with open(localconfig, 'r') as f:
		data = vdf.parse(f)
		appdata = data['UserLocalConfigStore']['Software']['valve']['Steam']['apps']

	def getlaunchoptions(appdata):
					appstr = "{\n"
					for key1, inner_dict in appdata.items():
									appstr += '"' + f"{key1}" + '"' + ": {\n"
									for key2, value in inner_dict.items():
													if key2 == "LaunchOptions":
														value = value.replace('"', '\\"')
														appstr += '"' + f"{key2}" + '"' + ": " + '"' + f"{value}" + '"\n'
									appstr += "},\n"
					appstr += "}"
					return appstr
	if len(sys.argv) > 1:
					appid = sys.argv[1]
					launchopt = sys.argv[2]
					appdata[appid]['LaunchOptions'] = launchopt
					vdf.dump(data, open(localconfig, 'w'), pretty=True)
					old, new = '},', '}'
					instance = getlaunchoptions(appdata).rfind(old)
					glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]
					with open(steamacsdir + "/launchoptions.json", 'w') as f:
									print(glo, file=f)
	else:
					old, new = '},', '}'
					instance = getlaunchoptions(appdata).rfind(old)
					glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]
					with open(steamacsdir + "/launchoptions.json", 'w') as f:
									print(glo, file=f)
