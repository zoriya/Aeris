import 'package:aeris/src/views/create_pipeline_page.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/widgets/aeris_page.dart';
import 'package:aeris/src/widgets/home_page_menu.dart';
import 'package:aeris/src/widgets/clickable_card.dart';
import 'package:aeris/src/widgets/home_page_sort_menu.dart';
import 'package:aeris/src/widgets/pipeline_card.dart';
import 'package:liquid_pull_to_refresh/liquid_pull_to_refresh.dart';
import 'package:provider/provider.dart';
import 'package:skeleton_loader/skeleton_loader.dart';

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
          floatingActionButton: FloatingActionButton(
            onPressed: () => showAerisCardPage(context, (_) => const CreatePipelinePage()),
            backgroundColor: Theme.of(context).colorScheme.secondary,
            child: const Icon(Icons.add),
          ),
          actions: [
            HomePageSortMenu(
              collectionProvider: provider,
            ),
            const HomePageMenu()
          ],
          body: provider.initialized == false
            ? ListView(physics: const BouncingScrollPhysics(),
              padding: const EdgeInsets.only(bottom: 20, top: 20, left: 10, right: 10),
              children: [SkeletonLoader(
                builder: ClickableCard(onTap:(){}, body: const SizedBox(height: 80)),
                items: 10,
                highlightColor: Theme.of(context).colorScheme.secondary
              )])
            : LiquidPullToRefresh(
            borderWidth: 2,
            animSpeedFactor: 3,
            color: Colors.transparent,
            showChildOpacityTransition: false,
            onRefresh: () => provider.fetchPipelines()
                .then((_) => setState(() {})), // refresh callback
            child: ListView.builder(
              physics: const BouncingScrollPhysics(),
              padding: const EdgeInsets.only(bottom: 20, top: 20, left: 10, right: 10),
              controller: listController,
              itemCount: provider.pipelineCount,
              itemBuilder: (BuildContext context, int index) =>
                PipelineCard(pipeline: provider.getPipelineAt(index),
            ),
          )),
    ));
  }
}
