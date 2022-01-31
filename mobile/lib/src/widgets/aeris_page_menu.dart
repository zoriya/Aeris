import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_popup_menu_item.dart';

/// Menu for the Home Page
class AerisPageMenu extends StatelessWidget {
  const AerisPageMenu({Key? key}) : super(key: key);


  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
      itemBuilder: (context) => [
        AerisPopupMenuItem(
          context: context, 
            icon: Icons.electrical_services, title: "Services", value: "/services"),
        AerisPopupMenuItem(
          context: context, icon: Icons.logout, title: "Logout", value: "/logout"),
      ],
      onSelected: (route) => Navigator.pushNamed(context, route as String),
      icon: Icons.more_horiz,
      menuOffset: const Offset(0, 50),
    );
  }
}
