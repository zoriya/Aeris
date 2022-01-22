import 'package:flutter/foundation.dart';
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
          Align(
            alignment: Alignment.bottomCenter,
            child: ElevatedButton(
              style: ElevatedButton.styleFrom(
                textStyle: const TextStyle(fontSize: 20),
                primary: Theme.of(context).colorScheme.primary,
              ),
              onPressed: () {
                Navigator.of(context).pushNamed('/login');
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
            strokeWidth: 2.15
          )
        ]
      )
    );
  }
}
