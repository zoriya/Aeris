import 'package:flutter/material.dart';

/// Menu for the Home Page
class AerisPageMenu extends StatelessWidget {
  const AerisPageMenu({Key? key}) : super(key: key);

  /// Function to create an item for the menu
  PopupMenuItem createMenuItem(
      IconData icon, String title, void Function() onTap) {
    return PopupMenuItem(
        child: Row(
          children: <Widget>[
            Container(child: Icon(
              icon,
              color: Colors.black,
            ), padding: const EdgeInsets.only(left: 10, right: 10)),
            Text(title),
          ],
        ),
        onTap: onTap);
  }

  @override
  Widget build(BuildContext context) {
    void Function(String) navigateTo = (String route) {
      Navigator.pushNamed(context, route);
    };
    return PopupMenuButton(
      itemBuilder: (context) => [
        // TODO Define nav routes
        createMenuItem(Icons.electrical_services, "Services", () => navigateTo("")),
        createMenuItem(Icons.logout, "Logout", () => navigateTo("")),
      ],
      offset: const Offset(0, 50),
      child: const Icon(Icons.more_horiz),
      shape: const RoundedRectangleBorder(
          borderRadius: BorderRadius.all(Radius.circular(20))),
    );
  }
}
