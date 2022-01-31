import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';

/// Menu for the Home Page
class AerisPageMenu extends StatelessWidget {
  const AerisPageMenu({Key? key}) : super(key: key);

  /// Function to create an item for the menu
  PopupMenuItem createMenuItem(
      IconData icon, String title, String route, BuildContext context) {
    return PopupMenuItem(
      value: route,
      child: Row(
        children: <Widget>[
          Container(
              child: Icon(
                icon,
                color: Theme.of(context).colorScheme.onSurface,
              ),
              padding: const EdgeInsets.only(left: 10, right: 10)),
          Text(title),
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
      itemBuilder: (context) => [
        createMenuItem(
            Icons.electrical_services, "Services", "/services", context),
        createMenuItem(Icons.logout, "Logout", "/logout", context),
      ],
      onSelected: (route) => Navigator.pushNamed(context, route as String),
      icon: Icons.more_horiz,
      menuOffset: const Offset(0, 50),
    );
  }
}
