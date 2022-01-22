/// Enumeration of Services that trigger pipelines
enum Service { youtube, discord, gmail, github, twitter, spotify }

// Class for a service (Youtube, Gmail, ...)
class Service {
  // Name of the service
  final String name;

  // URL To the service
  final String url;

  // URL To a service's logo
  final String logoUrl;

  const Service.spotify()
      : name = "Spotify",
        url = "https://www.spotify.com",
        logoUrl =
            "https://upload.wikimedia.org/wikipedia/commons/7/74/Spotify_App_Logo.svg";
  const Service.gmail()
      : name = "Gmail",
        url = "https://mail.google.com/",
        logoUrl =
            "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Gmail_Icon_%282013-2020%29.svg/2048px-Gmail_Icon_%282013-2020%29.svg.png";
  const Service.discord()
      : name = "Discord",
        url = "https://discord.com/app",
        logoUrl =
            "https://seeklogo.com/images/D/discord-logo-134E148657-seeklogo.com.png";
  const Service.twitter()
      : name = "Twitter",
        url = "https://twitter.com",
        logoUrl = "https://static.cdnlogo.com/logos/t/96/twitter-icon.svg";
  const Service.github()
      : name = "GitHub",
        url = "https://github.com/",
        logoUrl =
            "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg";
  const Service.youtube()
      : name = "Youtube",
        url = "https://youtube.com",
        logoUrl =
            "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/YouTube_full-color_icon_%282017%29.svg/159px-YouTube_full-color_icon_%282017%29.svg.png";
}
