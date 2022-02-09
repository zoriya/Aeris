import 'package:flutter/material.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/widgets/aeris_page.dart';
import 'package:aeris/src/widgets/home_page_menu.dart';
import 'package:aeris/src/widgets/clickable_card.dart';
import 'package:aeris/src/widgets/home_page_sort_menu.dart';
import 'package:aeris/src/widgets/pipeline_card.dart';
import 'package:liquid_pull_to_refresh/liquid_pull_to_refresh.dart';
import 'package:provider/provider.dart';

/// [StatefulWidget] used to display [HomePage] interface
class HomePage extends StatefulWidget {
  const HomePage({Key? key}) : super(key: key);

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  @override
  Widget build(BuildContext context) {
    ScrollController listController = ScrollController();
    return Consumer<PipelineProvider>(
      builder: (context, provider, _) => AerisPage(
          actions: [
            HomePageSortMenu(
              collectionProvider: provider,
            ),
            const HomePageMenu()
          ],
          body: LiquidPullToRefresh(
            borderWidth: 2,
            animSpeedFactor: 3,
            color: Colors.transparent,
            showChildOpacityTransition: false,
            onRefresh: () => Future.delayed(const Duration(seconds: 2))
                .then((_) => setState(() {
                      print("Loaded");

                      ///TODO Call api
                    })), // refresh callback
            child: ListView.builder(
              padding: const EdgeInsets.only(
                  top: 20, bottom: 20, left: 10, right: 10),
              controller: listController,
              itemCount: provider.pipelineCollection.pipelines.length + 1,
              itemBuilder: (BuildContext context, int index) {
                if (index == provider.pipelineCollection.pipelines.length) {
                  return ClickableCard(
                    color: Theme.of(context).colorScheme.secondary,
                    body: Padding(
                      padding: const EdgeInsets.only(top: 20, bottom: 20),
                      child: Text("Create a pipeline",
                          textAlign: TextAlign.center,
                          style: TextStyle(
                              color: Theme.of(context).colorScheme.onSecondary,
                              fontSize: 20,
                              fontWeight: FontWeight.w600)),
                    ),
                    onTap: () {
                      Navigator.of(context).pushNamed('/pipeline/new');
                    },
                  );
                }
                return PipelineCard(
                    pipeline: provider.pipelineCollection.pipelines[index]);
              },
            ),
          )),
    );
  }
}
