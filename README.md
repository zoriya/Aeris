[![Aeris](assets/logo-white.png)](assets/logo-white.png)

# Professional, personnal action-reaction manager

[![Build](https://github.com/AnonymusRaccoon/Aeris/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/AnonymusRaccoon/Aeris/actions/workflows/build.yml)

![API](https://img.shields.io/badge/API-Haskell-purple)
![Worker](https://img.shields.io/badge/Worker-Typescript-blue)
![Web Client](https://img.shields.io/badge/Web%20Client-React-turquoise)
![Mobile Client](https://img.shields.io/badge/Mobile%20Client-Flutter-lightblue)

## What is Aeris

Aeris is an Action-Reaction system manager. It lets its users create *Pipelines*.

Pipelines are triggered by an *Action* (for example, when a PR is open on some GitHub repository), and perfoms *Reactions* (for example, play a given song on Spotify).

To manage your Pipeline, you can use the Android App, as well as the Web client.

Aeris supports a variety of service, on which Pipelines are perfomed upon:

- GitHub
- Spotify
- Youtube
- Reddit
- Anilist
- Twitter

## What do I need to use Aeris?

Make sure the following softwares are installed on your machine:

- Docker, (and Docker-Compose)

To setup Aeris, you need to provide the building system some information:

- Client ID and Client Secrets for EACH services (fill the `.env.example` provided at the root of the repository and rename it `.env`)
- A Host name, to let clients know how to call the API (usually localhost:8080)

## How to install it ?

To install Aeris, run the following commands at the root of the repository:

- `docker-compose -f docker-compose.yml build` (grab a snack, it might take some time)
- `docker-compose -f docker-compose.yml up`

## Documentation

- [Mobile Client Documentation](https://anonymusraccoon.github.io/Aeris/)
- [API](https://github.com/AnonymusRaccoon/Aeris/blob/master/swagger.yaml)

## How to use it ?

The Aeris server is accessible on the host's 8080 port

You can access the Web client through port 8081

An Android APK can be downloaded via localhost:8081/client.apk

## Why *Aeris* ?

This project's name is inspired by [Aergia]( https://en.wikipedia.org/wiki/Aergia), the goddess of sloth.
This tool is an automatisation tool.
