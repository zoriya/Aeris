import 'package:aeris/src/aeris_api.dart';
import 'package:flutter_fadein/flutter_fadein.dart';
import 'package:flutter/material.dart';
import 'package:get_it/get_it.dart';
import '../widgets/aeris_page.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import '../../aeris.dart';

/// [StatefulWidget] used in order to display StartupPage [Widget]
class StartupPage extends StatefulWidget {
  const StartupPage({Key? key}) : super(key: key);

  @override
  _StartupPageState createState() => _StartupPageState();
}

class _StartupPageState extends State<StartupPage> {
  @override
  Widget build(BuildContext context) {
    bool isConnected = GetIt.I<AerisAPI>().isConnected;
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
          Padding(
            padding: const EdgeInsets.all(20),
            child: OverlayedText(
              text: AppLocalizations.of(context).aerisDescription,
              overlayedColor: const Color.fromRGBO(50, 0, 27, 1),
              textColor: const Color.fromRGBO(198, 93, 151, 1),
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
                if (isConnected) {
                  Navigator.of(context).popAndPushNamed('/home');
                } else {
                  Navigator.of(context).pushNamed('/login');
                }
              },
              child: Tooltip(
                message: 'Connexion',
                child: Text(AppLocalizations.of(context).connect)
              ),
            ),
          )
        ]
      )
    );
  }
}