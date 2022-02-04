// Class for a service (Youtube, Gmail, ...)
import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';


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

  const Service.spotify()
      : name = "Spotify",
        url = "https://www.spotify.com",
        logoUrl =
            "https://www.presse-citron.net/app/uploads/2020/06/spotify-une-.jpg";
  const Service.gmail()
      : name = "Gmail",
        url = "https://mail.google.com/",
        logoUrl =
            "https://play-lh.googleusercontent.com/KSuaRLiI_FlDP8cM4MzJ23ml3og5Hxb9AapaGTMZ2GgR103mvJ3AAnoOFz1yheeQBBI";
  const Service.discord()
      : name = "Discord",
        url = "https://discord.com/app",
        logoUrl =
            "https://play-lh.googleusercontent.com/fbrWR4LbtB_1Ulgz3_rw8bY3tx_zPU7A9ZOB5WYG_QmqOUUjA6JEzE_20GA4YBDWMx4";
  const Service.twitter()
      : name = "Twitter",
        url = "https://twitter.com",
        logoUrl =
            "https://f.hellowork.com/blogdumoderateur/2019/11/twitter-logo-1200x1200.jpg";
  const Service.github()
      : name = "GitHub",
        url = "https://github.com/",
        logoUrl = "https://avatars.githubusercontent.com/u/9919?s=280&v=4";
  const Service.youtube()
      : name = "Youtube",
        url = "https://youtube.com",
        logoUrl =
            "https://play-lh.googleusercontent.com/lMoItBgdPPVDJsNOVtP26EKHePkwBg-PkuY9NOrc-fumRtTFP4XhpUNk_22syN4Datc";

  /// Returns a list of all the available services
  static all() => const [
        Service.discord(),
        Service.github(),
        Service.gmail(),
        Service.youtube(),
        Service.twitter(),
        Service.spotify()
      ];
}
