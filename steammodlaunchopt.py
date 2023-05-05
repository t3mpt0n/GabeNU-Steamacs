import vdf
import json
import subprocess
import os
import sys

class SteamStuff:
	said0 = os.getenv("STEAM_ACCOUNT_ID")
	said1 = said0.rstrip().replace('"', '')
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
	storelaunchopts = os.getenv("HOME") + "/.emacs.d/steam/launchoptions.json"
	if len(sys.argv) > 1:
					appid = sys.argv[1]
					launchopt = sys.argv[2]
					appdata[appid]['LaunchOptions'] = launchopt
					vdf.dump(data, open(localconfig, 'w'), pretty=True)
					old, new = '},', '}'
					instance = getlaunchoptions(appdata).rfind(old)
					glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]
					with open("/home/jd/.emacs.d/steam/launchoptions.json", 'w') as f:
									print(glo, file=f)
	else:
					old, new = '},', '}'
					instance = getlaunchoptions(appdata).rfind(old)
					glo = getlaunchoptions(appdata)[:instance] + new + getlaunchoptions(appdata)[instance+len(old):]
					with open("/home/jd/.emacs.d/steam/launchoptions.json", 'w') as f:
									print(glo, file=f)
