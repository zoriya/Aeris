import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/aeris_page_menu.dart';
import 'package:mobile/src/widgets/background/animated_background.dart';

/// Application base page, holds scaffold and background
class AerisPage extends StatelessWidget {
  /// Body of the page
  final Widget body;
  /// Display appbar or not
  final bool displayAppbar;
  const AerisPage({Key? key, required this.body, this.displayAppbar = true}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SizedBox(
        width: MediaQuery.of(context).size.width,
        height: MediaQuery.of(context).size.height,
        child: Stack(
          children: <Widget>[
            const AnimatedBackground(),
            body
          ]
        )
      ),
      backgroundColor: Theme.of(context).colorScheme.primary,
      appBar: displayAppbar ? AppBar(
        title: const Text("AERIS"),
        centerTitle: true,
        elevation: 0,
        actions: const [
          Padding(child: AerisPageMenu(), padding: EdgeInsets.only(right: 10.0))
        ],
      ) : null ,
    );
  }
}
