import urllib.parse
import requests


class Geocoder:
    # Google API key
    gcode_key = None

    # String pattern for request
    request_pattern = 'https://maps.googleapis.com/maps/api/geocode/json?address={}&key={}'

    # Raw address input
    address = None

    # Response from Google API
    response = None

    # Is this a valid address?
    valid = False

    def __init__(self, address: str):
        self.gcode_key = 'AIzaSyCM_JiTBjn9hz5I2y4PnPEHEYIHDtb6kcw'

        self.address = address

        self.do_geocode()

    def do_geocode(self):
        print('Processing {}'.format(self.address))

        # URL encode the address
        parse_addr = urllib.parse.quote(str(self.address))

        # Send the request to Google
        request = self.request_pattern.format(parse_addr, self.gcode_key)

        # Get the response
        res = requests.get(request).json()

        # Get the response dictionary
        if isinstance(res['results'], dict):
            self.response = res['results']
        if not isinstance(res['results'], list) or len(res['results']) != 1:
            self.valid = False

            return
        else:
            self.response = res['results'][0]

        # Check if the response includes a street name - if so the address is valid
        self.valid = len([a for a in self.response['address_components'] if 'route' in a['types']]) > 0

    def get_response(self):
        return self.response

    def get_cleaned_address(self):
        if self.valid:
            return self.response['formatted_address']
        else:
            return 'No match'

    def get_address_components(self, component_types: [str]):
        if self.valid:
            address_components = self.response['address_components']

            component = [v['long_name'] for v in address_components if all(item in component_types for item in v['types'])]

            return component
        else:
            return 'No match'

    def get_lat_long(self):
        if self.valid:
            return [self.response['geometry']['location']['lat'], self.response['geometry']['location']['lng']]
        else:
            return [None, None]
