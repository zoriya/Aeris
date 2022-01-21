import 'package:flutter/material.dart';
import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:mobile/src/widgets/home_page/menu.dart';

/// Home Page
class HomePage extends StatelessWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return const AerisPage(body: HomePageMenu());
  }
}
