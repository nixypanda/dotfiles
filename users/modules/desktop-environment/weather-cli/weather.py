#!/home/sherub/.nix-profile/bin/python3

import json
from argparse import ArgumentParser
from datetime import datetime, timedelta
from http import HTTPStatus
from os import environ
from typing import Dict, Optional

import requests

APP_ID = environ["OPEN_WEATHER_API_KEY"]


def mk_parser():
    parser = ArgumentParser()
    parser.add_argument(
        "--display",
        type=str,
        choices=["icon", "temp", "hex", "stat", "quote"],
    )
    return parser


class OpenWeatherMapAPIWrapper:
    def get_new_data(self):
        response = requests.get(
            "https://api.openweathermap.org/data/2.5/weather",
            params={"q": "gurgaon", "appid": APP_ID, "units": "metric"},
        )
        if response.status_code == HTTPStatus.OK:
            return response.json()
        else:
            return None


class WeatherRepo:
    def store(self, weather: Dict):
        with open("/tmp/weather.json", "w") as f:
            f.write(json.dumps(weather, indent=4))

    def retrive(self) -> Optional[Dict]:
        try:
            with open("/tmp/weather.json", "r") as f:
                return json.load(f)
        except FileNotFoundError:
            return None


class WeatherService:
    def __init__(self, api: OpenWeatherMapAPIWrapper, repo: WeatherRepo) -> None:
        self.api = api
        self.repo = repo

    def get_current_data(self) -> Dict:
        weather = self.repo.retrive()

        if weather is None or is_stale(weather):
            new_info = self.api.get_new_data()
            if new_info is None:
                weather = {}
            else:
                new_info["updated_at"] = datetime.now().isoformat()
                weather = new_info
            self.repo.store(weather)

        return weather


def is_stale(weather):
    last_updated_at_str = weather.get("updated_at")
    if not last_updated_at_str:
        return True
    try:
        last_updated_at = datetime.fromisoformat(last_updated_at_str)
    except:
        return False
    else:
        return datetime.now() - last_updated_at > timedelta(hours=2)


def get_weather_service() -> WeatherService:
    return WeatherService(api=OpenWeatherMapAPIWrapper(), repo=WeatherRepo())


def weather_display_info(weather: Optional[Dict]):
    if weather is None:
        weather = {}

    w = weather.get("weather", [{}])[0]
    icon_code = w.get("icon")
    description = w.get("description", "Weather Unavailable")
    temp_celsius = weather.get("main", {}).get("temp", None)

    if icon_code is None:
        icon = " "
        quote = "Ah well, no weather huh? \nEven if there's no weather, it's gonna be a great day!"
        hex = "#adadff"
    elif icon_code == "50d":
        icon = " "
        quote = "Forecast says it's misty \nMake sure you don't get lost on your way..."
        hex = "#84afdb"
    elif icon_code == "50n":
        icon = " "
        quote = "Forecast says it's a misty night \nDon't go anywhere tonight or you might get lost..."
        hex = "#84afdb"
    elif icon_code == "01d":
        icon = " "
        quote = "It's a sunny day, gonna be fun! \nDon't go wandering all by yourself though..."
        hex = "#ffd86b"
    elif icon_code == "01n":
        icon = " "
        quote = (
            "It's a clear night \nYou might want to take a evening stroll to relax..."
        )
        hex = "#fcdcf6"
    elif icon_code == "02d":
        icon = " "
        quote = "It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
        hex = "#adadff"
    elif icon_code == "02n":
        icon = " "
        quote = "It's a cloudy night \nHow about some hot chocolate and a warm bed?"
        hex = "#adadff"
    elif icon_code == "03d":
        icon = " "
        quote = "It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
        hex = "#adadff"
    elif icon_code == "03n":
        icon = " "
        quote = "It's a cloudy night \nHow about some hot chocolate and a warm bed?"
        hex = "#adadff"
    elif icon_code == "04d":
        icon = " "
        quote = "It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
        hex = "#adadff"
    elif icon_code == "04n":
        icon = " "
        quote = "It's a cloudy night \nHow about some hot chocolate and a warm bed?"
        hex = "#adadff"
    elif icon_code == "09d":
        icon = " "
        quote = "It's rainy, it's a great day! \nGet some ramen and watch as the rain falls..."
        hex = "#6b95ff"
    elif icon_code == "09n":
        icon = " "
        quote = " It's gonna rain tonight it seems \nMake sure your clothes aren't still outside..."
        hex = "#6b95ff"
    elif icon_code == "10d":
        icon = " "
        quote = "It's rainy, it's a great day! \nGet some ramen and watch as the rain falls..."
        hex = "#6b95ff"
    elif icon_code == "10n":
        icon = " "
        quote = " It's gonna rain tonight it seems \nMake sure your clothes aren't still outside..."
        hex = "#6b95ff"
    elif icon_code == "11d":
        icon = ""
        quote = (
            "There's storm for forecast today \nMake sure you don't get blown away..."
        )
        hex = "#ffeb57"
    elif icon_code == "11n":
        icon = ""
        quote = "There's gonna be storms tonight \nMake sure you're warm in bed and the windows are shut..."
        hex = "#ffeb57"
    elif icon_code == "13d":
        icon = " "
        quote = "It's gonna snow today \nYou'd better wear thick clothes and make a snowman as well!"
        hex = "#e3e6fc"
    elif icon_code == "13n":
        icon = " "
        quote = "It's gonna snow tonight \nMake sure you get up early tomorrow to see the sights..."
        hex = "#e3e6fc"
    elif icon_code == "40d":
        icon = " "
        quote = "Forecast says it's misty \nMake sure you don't get lost on your way..."
        hex = "#84afdb"
    elif icon_code == "40n":
        icon = " "
        quote = "Forecast says it's a misty night \nDon't go anywhere tonight or you might get lost..."
        hex = "#84afdb"
    else:
        icon = " "
        quote = "Sort of odd, I don't know what to forecast \nMake sure you have a good time!"
        hex = "#adadff"

    return {
        "description": description,
        "hex": hex,
        "icon": icon,
        "quote": quote,
        "temp": f"{temp_celsius}°C",
    }


if __name__ == "__main__":
    parser = mk_parser()
    args = parser.parse_args()

    weather_repo = get_weather_service()
    weather = weather_repo.get_current_data()
    display = weather_display_info(weather)

    if args.display == "icon":
        print(display["icon"])
    elif args.display == "hex":
        print(display["hex"])
    elif args.display == "temp":
        print(display["temp"])
    elif args.display == "quote":
        print(display["quote"])
    elif args.display == "stat":
        print(display["description"])


example = {
    "coord": {"lon": 77.0333, "lat": 28.4667},
    "weather": [{"id": 701, "main": "Mist", "description": "mist", "icon": "50d"}],
    "base": "stations",
    "main": {
        "temp": 291.17,
        "feels_like": 291.48,
        "temp_min": 291.17,
        "temp_max": 291.17,
        "pressure": 1016,
        "humidity": 94,
    },
    "visibility": 2200,
    "wind": {"speed": 4.12, "deg": 130},
    "clouds": {"all": 75},
    "dt": 1641556346,
    "sys": {
        "type": 1,
        "id": 9165,
        "country": "IN",
        "sunrise": 1641519916,
        "sunset": 1641557418,
    },
    "timezone": 19800,
    "id": 1270642,
    "name": "Gurgaon",
    "cod": 200,
}
