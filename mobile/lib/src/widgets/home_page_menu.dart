import 'package:aeris/src/views/service_page.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/widgets/aeris_popup_menu.dart';
import 'package:aeris/src/widgets/aeris_popup_menu_item.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

/// Menu for the Home Page
class HomePageMenu extends StatelessWidget {
  const HomePageMenu({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
      itemBuilder: (context) => [
        AerisPopupMenuItem(
            context: context,
            icon: Icons.electrical_services,
            title: AppLocalizations.of(context).services,
            value: "/services"),
        AerisPopupMenuItem(
            context: context,
            icon: Icons.logout,
            title: AppLocalizations.of(context).logout,
            value: "/logout"), ///TODO Logout
      ],
      onSelected: (route) {
        if (route as String == "/services") {
          showAerisCardPage(context, (_) => const ServicePage());
        }
        },
      icon: Icons.more_horiz,
      menuOffset: const Offset(0, 50),
    );
  }
}
