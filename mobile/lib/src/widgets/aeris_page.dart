import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/home_page_menu.dart';
import 'package:mobile/src/widgets/background/animated_background.dart';

/// Application base page, holds scaffold and background
class AerisPage extends StatelessWidget {
  /// Body of the page
  final Widget body;

  /// Display appbar or not
  final bool displayAppbar;

  /// Actions for appbar
  final List<Widget> actions;
  const AerisPage(
      {Key? key,
      required this.body,
      this.displayAppbar = true,
      this.actions = const []})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SizedBox(
          width: MediaQuery.of(context).size.width,
          height: MediaQuery.of(context).size.height,
          child: Stack(children: <Widget>[const AnimatedBackground(), body])),
      backgroundColor: Theme.of(context).colorScheme.primary,
      appBar: displayAppbar
          ? AppBar(
              title: const Text("AERIS"),
              centerTitle: true,
              elevation: 0,
              leading: Container(),
              actions: [
                for (Widget action in actions)
                  Padding(
                      child: action,
                      padding: const EdgeInsets.only(right: 10.0)),
              ],
            )
          : null,
    );
  }
}
