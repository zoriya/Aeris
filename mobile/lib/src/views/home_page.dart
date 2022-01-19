import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/overlayed_texts.dart';

class HomePage extends StatelessWidget {

  const HomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        body: Stack(
          children: <Widget>[
            // Container(
            //   alignment: Alignment.center,
            //   child: CachedNetworkImage(
            //     imageUrl: "https://cdn.dribbble.com/users/244018/screenshots/1506924/media/b0f10339a5471a0733561a83636babf8.gif",
            //     imageBuilder: (context, imageProvider) => Container(
            //       decoration: BoxDecoration(
            //           image: DecorationImage(
            //             image: imageProvider,
            //             fit: BoxFit.cover,
            //             colorFilter: ColorFilter.mode(Colors.black.withOpacity(0.9), BlendMode.dstATop),
            //           )
            //       ),
            //     ),
            //   ),
            // ),
            Align(
              alignment: Alignment.bottomCenter,
              child: ElevatedButton(
                style: ElevatedButton.styleFrom(
                  textStyle: const TextStyle(
                      fontSize: 20
                  ),
                  primary: Theme.of(context).colorScheme.primary,
                ),
                onPressed: () {
                  print("On clique sur 'se connecter'!");
                },
                child: const Tooltip(
                    message: 'Connexion',
                    child: Text("Se connecter")
                ),
              ),
            ),
            OverlayedText(
              text: "Aeris your new Action Dashboard!",
              overlayedColor: Colors.black,
              textColor: Theme.of(context).colorScheme.secondary,
              fontSize: 20,
              strokeWidth: 1.95
            )
          ],
        )
    );
  }
}