import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

/// Menu for the Home Page
class AerisPageMenu extends StatelessWidget {
  const AerisPageMenu({Key? key}) : super(key: key);

  /// Function to create an item for the menu
  PopupMenuItem createMenuItem(
      IconData icon, String title, void Function() onTap) {
    return PopupMenuItem(
        child: ListTile(leading: Icon(icon), title: Text(title)), onTap: onTap);
  }

  @override
  Widget build(BuildContext context) {
    void Function(String) navigateTo = (String route) {
      Navigator.pushNamed(context, route);
    };
    return PopupMenuButton(
      itemBuilder: (context) =>
          [createMenuItem(Icons.access_time, "Hello", () => navigateTo("/"))],
      offset: const Offset(0, 50),
    );
  }
}
