import 'package:flutter/material.dart';
import 'package:aeris/src/widgets/background/animated_background.dart';

/// Application base page, holds scaffold and background
class AerisPage extends StatelessWidget {
  /// Body of the page
  final Widget body;

  /// Display appbar or not
  final bool displayAppbar;

  final Widget? floatingActionButton;

  /// Actions for appbar
  final List<Widget> actions;
  const AerisPage(
      {Key? key,
      required this.body,
      this.displayAppbar = true,
      this.floatingActionButton,
      this.actions = const []})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      floatingActionButton: floatingActionButton,
      resizeToAvoidBottomInset: false,
      body: SizedBox(
          width: MediaQuery.of(context).size.width,
          height: MediaQuery.of(context).size.height,
          child: Stack(children: <Widget>[const AnimatedBackground(), body])),
      backgroundColor: Theme.of(context).colorScheme.primary,
      appBar: displayAppbar
          ? AppBar(
              title: const Text("AERIS"),
              centerTitle: false,
              elevation: 0,
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
