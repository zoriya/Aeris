import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/home_page/menu.dart';

/// Application base page, holds scaffold and background
class AerisPage extends StatelessWidget {
  /// Body of the page
  final Widget body;
  const AerisPage({Key? key, required this.body}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        body: body,
        appBar: AppBar(
          title: const Text("AERIS"),
          titleSpacing: 20,
          actions: const [Padding(child: HomePageMenu(), padding: EdgeInsets.only(right: 10.0),)],
        ),
    );
  }
}