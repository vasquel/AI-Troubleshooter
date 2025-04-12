:- module(rules, [rule/3]).
% rule(Category, KeywordsList, Action).

rule('Display and Video Issues',
     ['display', 'no image', 'hdmi', 'screen', 'black', 'video'],
     'Check that the HDMI cable is properly connected or not damaged.').

rule('Display and Video Issues',
     ['image', 'video', 'no audio', 'sound missing', 'hdmi'],
     'Properly configure the audio output on the source device.').

rule('Display and Video Issues',
     ['mode not supported', 'resolution', 'hdmi', 'display error'],
     'Adjust the resolution of the external device to a compatible one.').

rule('Display and Video Issues',
     ['screen', 'blinking', 'dark', 'flicker', 'power', 'dim'],
     'Disable energy-saving features from the settings menu.').

rule('Display and Video Issues',
     ['black and white', 'color issue', 'monochrome'],
     'Check if grayscale mode is enabled.').

rule('Power and Shutdown Issues',
     ['auto shutdown', 'turns off', 'sleep', 'power saving'],
     'Check if timers or energy-saving mode are active.').

rule('Power and Shutdown Issues',
     ['hot', 'warm', 'temperature', 'overheating'],
     'It\'s normal for it to get warm during use, as long as there\'s no smell or smoke.').

rule('Power and Shutdown Issues',
     ['settings reset', 'config lost', 'not saving'],
     'Perform a factory reset to restore settings properly.').

rule('Audio Issues',
     ['dvi', 'no audio', 'no sound', 'hdmi', 'adapter', 'output'],
     'An additional cable is required to transmit audio.').

rule('Audio Issues',
     ['low volume', 'no sound', 'maximum volume', 'mute'],
     'Check the audio output settings of the external device.').

rule('Audio Issues',
     ['voice', 'narration', 'screen reader', 'talking', 'audio'],
     'Disable the voice guide function from accessibility settings.').

rule('Audio Issues',
     ['voice control', 'mic not working', 'bixby', 'alexa', 'unresponsive'],
     'Re-pair the remote control to use voice commands.').

rule('Connectivity Issues',
     ['wifi', 'can\'t connect', 'network', 'ssid', 'internet'],
     'Check the Wi-Fi password or possible interference.').

rule('Remote and Interaction Issues',
     ['remote', 'not working', 'no response', 'battery', 'sync'],
     'Check the remote batteries and re-pair it with the device if necessary.').

rule('Other Issues',
     ['plastic smell', 'burn', 'new'],
     'It is a temporary condition that will disappear after a few hours of use.').
