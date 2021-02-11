run:
	stack run
	arduino-cli compile --fqbn arduino:avr:uno ArduinoLights
	arduino-cli upload -p /dev/cu.usbmodem121201 --fqbn arduino:avr:uno ArduinoLights

setup:
	brew install arduino-cli
	arduino-cli core update-index
	arduino-cli board list
	arduino-cli core install arduino:avr
	arduino-cli core list
