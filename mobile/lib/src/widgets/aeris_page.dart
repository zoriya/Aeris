import 'package:flutter/material.dart';

/// Application base page, holds scaffold and background
class AerisPage extends StatelessWidget {
  /// Body of the page
  final Widget body;
  const AerisPage({Key? key, required this.body}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        body: body,
        backgroundColor: Theme.of(context).colorScheme.primary,
    );
  }
}