// Class for a service (Youtube, Gmail, ...)
import 'package:aeris/src/aeris_api.dart';
import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'dart:core';

import 'package:get_it/get_it.dart';

/// Data class used to store data about a service (logo, url, name)
class Service {
  ///Name of the service
  final String name;

  ///URL To the service
  final String url;

  ///URL To a service's logo
  final String logoUrl;

  Widget getLogo({double logoSize = 40}) => ClipRRect(
      borderRadius: BorderRadius.circular(8.0),
      child: CachedNetworkImage(
        fit: BoxFit.contain,
        imageUrl: logoUrl,
        width: logoSize,
        height: logoSize,
      ));

  /// Get full url for OAuth2
  String get authUrl => GetIt.I<AerisAPI>().getServiceAuthURL(this);

  const Service.spotify()
      : name = "Spotify",
        url = "https://www.spotify.com",
        logoUrl =
            "https://www.presse-citron.net/app/uploads/2020/06/spotify-une-.jpg";
  const Service.anilist()
      : name = "Anilist",
        url = "https://anilist.co",
        logoUrl =
            "https://anilist.co/img/icons/android-chrome-512x512.png";
  const Service.reddit()
      : name = "Reddit",
        url = "https://www.reddit.com",
        logoUrl =
            "https://www.elementaryos-fr.org/wp-content/uploads/2019/08/logo-reddit.png"; ///TODO Get icon
  const Service.twitter()
      : name = "Twitter",
        url = "https://twitter.com",
        logoUrl =
            "https://f.hellowork.com/blogdumoderateur/2019/11/twitter-logo-1200x1200.jpg";
  const Service.github()
      : name = "Github",
        url = "https://github.com/",
        logoUrl = "https://avatars.githubusercontent.com/u/9919?s=280&v=4";
  const Service.youtube()
      : name = "Youtube",
        url = "https://youtube.com",
        logoUrl =
            "https://play-lh.googleusercontent.com/lMoItBgdPPVDJsNOVtP26EKHePkwBg-PkuY9NOrc-fumRtTFP4XhpUNk_22syN4Datc";
  const Service.utils()
      : name = "Utils",
        url = "",
        logoUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Cle.png/1024px-Cle.png";

  /// Returns a list of all the available services
  static List<Service> all() => const [
        Service.reddit(),
        Service.github(),
        Service.anilist(),
        Service.youtube(),
        Service.twitter(),
        Service.spotify(),
        Service.utils(),
      ];

  /// Construct a service based on a lowercase string, the name of the service
  static Service factory(String name) {
    if (name.toLowerCase() == "git") return const Service.github();
    if (name.toLowerCase() == "ani") return const Service.anilist();
    for (Service service in Service.all()) {
      if (service.name.toLowerCase() == name.toLowerCase()) return service;
    }
    throw Exception("Unknown service");
  }
}
