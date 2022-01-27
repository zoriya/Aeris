import 'package:flutter_fadein/flutter_fadein.dart';
import 'package:flutter/material.dart';
import '../widgets/aeris_page.dart';

import '../../aeris.dart';

class StartupPage extends StatefulWidget {
  const StartupPage({Key? key}) : super(key: key);

  @override
  _StartupPageState createState() => _StartupPageState();
}

class _StartupPageState extends State<StartupPage> {
  @override
  Widget build(BuildContext context) {
    return AerisPage(
      displayAppbar: false,
      body: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Padding(
            padding: const EdgeInsets.only(bottom: 40),
            child: FadeIn(
              child: Image.asset('assets/logo.png'),
              duration: const Duration(seconds: 1),
              curve: Curves.easeInOut
            )
          ),
          const Padding(
            padding: EdgeInsets.all(70),
            child: OverlayedText(
              text: "Aeris is the best AREA in Nantes! Control each of your social network with Aeris, your new Action / Reaction app.",
              overlayedColor: Color.fromRGBO(50, 0, 27, 1),
              textColor: Color.fromRGBO(198, 93, 151, 1),
              fontSize: 20,
              strokeWidth: 2.15
            )
          ),
          Align(
            alignment: Alignment.bottomCenter,
            child: ElevatedButton(
              style: ElevatedButton.styleFrom(
                textStyle: const TextStyle(fontSize: 20),
                primary: Theme.of(context).colorScheme.secondary,
              ),
              onPressed: () {
                Navigator.of(context).pushNamed('/login');
              },
              child: const Tooltip(
                message: 'Connexion',
                child: Text("Se connecter")
              ),
            ),
          )
        ]
      )
    );
  }
}