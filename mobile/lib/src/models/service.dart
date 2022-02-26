// Class for a service (Youtube, Gmail, ...)
import 'dart:math';

import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:flutter_dotenv/flutter_dotenv.dart';
import 'dart:core';

/// Data class used to store data about a service (logo, url, name)
class Service {
  ///Name of the service
  final String name;

  ///URL To the service
  final String url;

  ///URL To a service's logo
  final String logoUrl;

  ///return the url to authenticate to service via OAuth2
  final String authUrl;

  Widget getLogo({double logoSize = 40}) => ClipRRect(
      borderRadius: BorderRadius.circular(8.0),
      child: CachedNetworkImage(
        fit: BoxFit.contain,
        imageUrl: logoUrl,
        width: logoSize,
        height: logoSize,
      ));

  static String _generateRandomString() {
    var randomString = "";
    var seed = Random();
    var randomNumber = seed.nextInt(10) * 10;

    for (var i = 0; i < 20 + randomNumber; i++) {
      randomString += String.fromCharCode(33 + (seed.nextInt(10) * 94));
    }
    return randomString;
  }

  Service.spotify()
      : name = "Spotify",
        url = "https://www.spotify.com",
        logoUrl =
            "https://www.presse-citron.net/app/uploads/2020/06/spotify-une-.jpg",
        authUrl =
            "https://accounts.spotify.com/authorize?client_id=${dotenv.env['SPOTIFY_CLIENT_ID']}&response_type=code&redirect_uri=https://localhost:3000/authorization/spotify";
  Service.gmail()
      : name = "Gmail",
        url = "https://mail.google.com/",
        logoUrl =
            "https://play-lh.googleusercontent.com/KSuaRLiI_FlDP8cM4MzJ23ml3og5Hxb9AapaGTMZ2GgR103mvJ3AAnoOFz1yheeQBBI",
        authUrl = "";

  ///TODO find
  Service.discord()
      : name = "Discord",
        url = "https://discord.com/app",
        logoUrl =
            "https://play-lh.googleusercontent.com/fbrWR4LbtB_1Ulgz3_rw8bY3tx_zPU7A9ZOB5WYG_QmqOUUjA6JEzE_20GA4YBDWMx4",
        authUrl =
            "https://discord.com/api/oauth2/authorize?response_type=code&client_id=${dotenv.env['DISCORD_CLIENT_ID']}&scope=applications.commands%20applications.entitlements%20applications.store.update%20bot%20guilds%20guilds.join%20guilds.members.read%20identify%20messages.read%20webhook.incoming&state=${_generateRandomString()}";
  Service.twitter()
      : name = "Twitter",
        url = "https://twitter.com",
        logoUrl =
            "https://f.hellowork.com/blogdumoderateur/2019/11/twitter-logo-1200x1200.jpg",
        authUrl =
            "https://twitter.com/i/oauth2/authorize?response_type=code&client_id=${dotenv.env['TWITTER_CLIENT_ID']}&redirect_uri=https://localhost:3000/authorization/twitter&scope=tweet.read%20users.read%20offline.access&state=${_generateRandomString()}&code_challenge=challenge&code_challenge_method=plain";
  Service.github()
      : name = "GitHub",
        url = "https://github.com/",
        logoUrl = "https://avatars.githubusercontent.com/u/9919?s=280&v=4",
        authUrl =
            "https://github.com/login/oauth/authorize?client_id=${dotenv.env['GITHUB_CLIENT_ID']}&response_type=code&redirect_uri=http://localhost:3000/authorization/github";
  Service.youtube()
      : name = "Youtube",
        url = "https://youtube.com",
        logoUrl =
            "https://play-lh.googleusercontent.com/lMoItBgdPPVDJsNOVtP26EKHePkwBg-PkuY9NOrc-fumRtTFP4XhpUNk_22syN4Datc",
        authUrl =
            "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=${dotenv.env['GOOGLE_CLIENT_ID']}&scope=openid%20email&redirect_uri=http://localhost:3000/authorization/google&state=${_generateRandomString()}";

  /// Returns a list of all the available services
  static all() => [
        Service.discord(),
        Service.github(),
        Service.gmail(),
        Service.youtube(),
        Service.twitter(),
        Service.spotify()
      ];

  static Service factory(String name) {
    for (Service service in Service.all()) {
      if (service.name.toLowerCase() == name.toLowerCase()) return service;
    }
    throw Exception("Unknown service");
  }
}
