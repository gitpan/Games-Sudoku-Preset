use strict;
use warnings;
#use feature 'say';

package Games::Sudoku::Preset;

use version; our $VERSION = qv('0.0.2');    # PBP

use Tk;
use List::Util qw(first);

my @cells;   # array of cell objects (0 .. 80)

# ====================================================================
#   Start methods stuff
# ====================================================================

# Start method for entering a new puzzle
# Usage: my $puzzle = Games::Sudoku::Preset->enter();
#        $puzzle: the entered and validated puzzle as an 81 character string
#
sub enter {
	my $class = shift;
#	createGUI();
	initGUI();

    Tk::MainLoop();
	my $game = mk_result();
    return $game;
}

# Start method validate
#   Purpose: validate a sudoku puzzle
#   Usage:   my $puzzle = Games::Sudoku::Preset->validate($game);
#   Result:  validated puzzle as a string of 81 characters
#
sub validate {
	my ($class, $game) = @_;

	my $err_ref = eval_initGUI_with_game($game);

    Tk::MainLoop() if ($err_ref);
	$game = mk_result();
    return $game;
}

# Start method edit
#   Purpose: edit a sudoku puzzle
#   Usage:   my $puzzle = Games::Sudoku::Preset->edit($game);
#   Result:  edited and validated puzzle as a string of 81 characters
#
sub edit {
	my ($class, $game) = @_;

	my $err_ref = eval_initGUI_with_game($game);

    Tk::MainLoop();
	$game = mk_result();
    return $game;
}

sub eval_initGUI_with_game {
	my $game = shift;

	my $err_ref = eval {initGUI_with_game($game)};
    if ($@) {
		print STDERR "Fatal error: $@\n";
		$err_ref = 1;
	}
    return $err_ref;
}

sub initGUI_with_game {
	my $game = shift;

	$game = purify($game);
	return '' unless $game;   # after wrong ref type
	my $holder = first {$_ !~ /([1-9])/ } (split '', $game);
	'Cell'->placeholder($holder);   # keep for return
	initGUI();
	my $count;
	$count++ while $game =~ /[1-9]/g;
	show_initial_count($count);
	my $err_ref = verify_game($game);
	mark_problem_cells($err_ref) if $err_ref;
	return $err_ref;
}

# convert the sudoku board to a string of 81 characters
# and return this to the caller (a start method).
# This becomes the return value of the start method
#
sub mk_result {
    # placeholder for unknown digits in sudoku output files
    my $unknown_digit = 'Cell'->placeholder();

    my @alldigits = map( {
	                        $_->{Value} || $unknown_digit; 
						  } @cells );
        return join '', @alldigits;
}

# ====================================================================
#   GUI stuff
# ====================================================================

{   # start of GUI block


my $mw;               # the MainWindow
my $tinysize = 10;    # size of a tiny square (pixels)
my $fieldsize;        # size of a sudoku field
my $activefield_index = -1;  # the currently active sudoku field
my $clickfield;              # the toplevel which covers the active sudoku field
                             # for clicking
my @tiny_fields = (undef);   # the tiny squares of the clickfield (indexed 1 .. 9)
my $valuecount  = 0;         # count of entered values
my $status_lb;               # the status Label

sub initGUI {
#   $mw = shift;

	$mw or $mw = MainWindow->new();
	createGUI();
	return;
}

sub createGUI {

	$mw or $mw = MainWindow->new();
	# let a click on the kill button (at the right side of the titlebar)
	# cancel the program
    $mw->protocol( 'WM_DELETE_WINDOW', \&Tk::exit );
    $fieldsize = 3 * ( $tinysize + 1 ) - 1;    # size of sudoku field
    my $totalsize = 9 * ( $fieldsize + 1 ) - 1;
    create_board($totalsize);
	
    # create bottom area

    my $but_fr = $mw->Frame()->pack( -side => 'bottom', -fill => 'x' );

    # make clickfield invisible while mouse is over the bottom frame
    $but_fr->bind( '<Enter>', sub { $clickfield->withdraw } );
    my $stat_fr = $but_fr->Frame()->pack( -fill => 'x' );

    # create value count labels

    $stat_fr->Label( -text => 'values' )->pack( -side => 'right' );
    $stat_fr->Label(
        -textvariable => \$valuecount,
        -width        => 2,
        -anchor       => 'e'
    )->pack( -side => 'right' );

	# create status label

    $status_lb = $stat_fr->Label()->pack( -fill => 'x' );

    # create Done button

    my $done_b = $but_fr->Button(
        -text    => 'Done',
        -command => sub {
                         my $err = verify_board();
                         $err or { $mw->destroy() };
						},
    )->pack( -side => 'left', -padx => 10, -pady => 3 );

    # create Save&Cancel button

    $but_fr->Button(
        -text    => 'Save & Cancel',
        -command => sub { my $ok = save_sudoku($mw);
                         $ok and Tk::exit() ;
						},
    )->pack( -side => 'left', -padx => 10, -pady => 3 );

    # create Cancel button
    $but_fr->Button( -text => 'Cancel', -command => sub { Tk::exit() } )
      ->pack( -side => 'left' );

    # set window size

    my $size_y = $totalsize + $done_b->reqheight + $status_lb->reqheight + 6;
    $mw->geometry("${totalsize}x$size_y");
    $mw->resizable( 0, 0 );    # freeze window size
    create_clickfield($mw);

    # On Windows XP, the sudoku window likes to hide itself behind the "DOS"
    # shell window at the moment where the clickfield pops up for the 2nd time.
    # This can be avoided by
    # $mw->raise() or $mw->focus() or $cells[any]->{Button}->focus().
    # Set the initial focus to the 1st field
    $cells[0]->{Button}->focus();

	return;
}

sub create_board {
    my $totalsize = shift;

	my $field_index = 0;
    foreach my $i ( 0 .. 8 ) {
        foreach my $j ( 0 .. 8 ) {
            my $fieldID = create_field( $j, $i );
			my $cellobj = 'Cell'->new($field_index, $fieldID);
            $fieldID->configure(-textvariable => \$cellobj->{Value});
            push @cells, $cellobj;
            $field_index++;
        }
    }

    # draw block separator lines
    foreach my $pos ( 3, 6 ) {
        my $where = $pos * ( $fieldsize + 1 ) - 1;
        $mw->Frame(
            -width      => 1,
            -height     => $totalsize,
            -background => 'black'
        )->place( -x => $where, -y => 0 );
        $mw->Frame(
            -width      => $totalsize,
            -height     => 1,
            -background => 'black'
        )->place( -x => 0, -y => $where );
    }
    return;
}

# create a sudoku field
#
sub create_field {
    my ( $w, $h ) = my ( $w_num, $h_num ) = @_;    # pos. num.s of sudoku field (0 .. 8)
    my $field_index = $w + 9 * $h;                 # index of sudoku field (0 .. 80)
    $w *= $fieldsize + 1;                          # pos. of sudoku field (pixels)
    $h *= $fieldsize + 1;                          #

    my $space = $mw->Frame( -width => $fieldsize, -height => $fieldsize )
      ->place( -x => $w, -y => $h );
    $space->packPropagate(0);                      # prevent resizing the frame
    my $fieldID = $space->Button()->pack( -fill => 'both', -expand => 1 );

    # mouse and keyboard bindings

    $fieldID->bind( '<Enter>', [ \&move_clickfield ] );
    foreach my $digit ( 1 .. 9 ) {
        #alpha keypad
        $fieldID->bind(
            "<Key-$digit>" => [ \&change_digit, $digit ] );

        #numeric keypad
        $fieldID->bind(
            "<KP_$digit>" => [ \&change_digit, $digit ] );
    }
    # delete digit
    foreach my $key (qw/0 KP_0 space Delete/) {
        $fieldID->bind( "<$key>" => [ \&change_digit ] );
    }

	# keyboard focus move
    $fieldID->bind( "<Key-Up>"    => [ \&move_focus, $w_num,     $h_num - 1 ] );
    $fieldID->bind( "<Key-Down>"  => [ \&move_focus, $w_num,     $h_num + 1 ] );
    $fieldID->bind( "<Key-Left>"  => [ \&move_focus, $w_num - 1, $h_num ] );
    $fieldID->bind( "<Key-Right>" => [ \&move_focus, $w_num + 1, $h_num ] );
    return $fieldID;
}

# move focus to neighbouring sudoku field
# callback of the arrow keys
#
sub move_focus {
    my ( $fieldID, $w_new, $h_new ) = @_;

    $clickfield->withdraw;    # make clickfield invisible
    $w_new %= 9;              # end-around
    $h_new %= 9;
    $cells[ $w_new + 9 * $h_new ]->{Button}->focus();
    return;
}

# delete resp. replace sudoku digit
# callback of some keys (alpha or numeric keypad)
# also called from change_my_digit
#
sub change_digit {
    my ( $fieldID, $digit_num ) = @_;   # ID of button, digit

    $clickfield->withdraw;    # make clickfield invisible
	reset_colors() if $status_lb->cget('-fg') eq 'red';
	
    if ($digit_num) {
        # set or replace old digit
        ${$fieldID->cget('-textvariable')} or $valuecount++;
        ${$fieldID->cget('-textvariable')} = $digit_num;
    }
    else {
        # delete old digit
        ${$fieldID->cget('-textvariable')} = '';
        $valuecount--;
    }

    return;
}

sub show_initial_count {
	$valuecount = shift;
	return;
}

# ====================================================================
# clickfield stuff
# ====================================================================

sub create_clickfield {
	my $mw = shift;

    $clickfield = $mw->Toplevel( -width => $fieldsize, -height => $fieldsize );
    $clickfield->overrideredirect(1);    # suppress window frame
    foreach my $i ( 0 .. 2 ) {
        foreach my $j ( 0 .. 2 ) {
            create_tinysquare( $j, $i );
        }
    }
    $clickfield->withdraw;   # make clickfield invisible
	# make clickfield invisible when the window gets moved
	$mw->bind( '<Configure>' => sub { $clickfield->withdraw;} );

    return;
}

sub create_tinysquare {
    my ($w, $h ) = my ($w_num, $h_num ) 
	              = @_;    # pos. num.s of tiny square
    $w *= $tinysize + 1;   # pos. of tiny square
    $h *= $tinysize + 1;   #

    my $space = $clickfield->Frame( -width => $tinysize, -height => $tinysize )
      ->place( -x => $w, -y => $h );
    $space->packPropagate(0);    # prevent resizing the frame
    my $tiny = $space->Button(
        -relief     => 'flat',
        -background => 'black',
        -command    => [ \&change_my_digit, $w_num + 3 * $h_num + 1 ],
    )->pack( -fill => 'both', -expand => 1 );
    push( @tiny_fields, $tiny );
    return;
}

# position the clickfield over the entered sudoku field
# callback of the <Enter> event
#
sub move_clickfield {
    my ( $fieldID ) = shift;   # ID of button to be covered

    # ignore re-entering the active field
    # (this happens when withdrawing the clickfield)

    # Color change and popup required when returning from the bottom row,
    # so no return in this case
    return
      if (  $fieldID == ('Cell'->activefield() || 0)
        and $clickfield->state eq 'normal' );

    'Cell'->activefield($fieldID);
    $clickfield->withdraw;    # make clickfield invisible

    # mark the tiny square of the current digit by a different color
    foreach my $tiny ( @tiny_fields[ 1 .. 9 ] ) {
        $tiny->configure(
            -background       => 'black',
            -activebackground => 'black'
        );
    }
    if ( my $digit = ${'Cell'->activefield()->cget('-textvariable')} ) {
        $tiny_fields[$digit]->configure(
            -background       => 'red',
            -activebackground => 'orange'
        );
    }
    $clickfield->configure( -popover => $fieldID );
    $clickfield->Popup();    # make clickfield visible
    return;
}

# delete resp. replace old digit of the active sudoku field
# callback of the tiny squares
#
sub change_my_digit {
    my $digit_num = shift;    # digit of the clicked tiny square
    my $actfield = 'Cell'->activefield();
    my $olddigit = ${$actfield->cget('-textvariable')};
    if ( $olddigit eq $digit_num ) { $digit_num = undef };
    change_digit( $actfield, $digit_num );
    return;
}

# ====================================================================

# show problem cells in red on the board.
#
sub mark_problem_cells {
	my $err_ref = shift;
	my ($errtxt, $errcells_ref) = @$err_ref;
	$status_lb->configure( -text => $errtxt, -fg => 'red');
	foreach my $errcell (@$errcells_ref) {
		my $fieldID = $errcell->{Button};
	    if ($errcell->{Value}) {
			$fieldID->configure(-fg => 'red');
		} else {
			$fieldID->configure(-bg => 'red');
		}
		$errcell->is_errcell(1);
	}
    return;
}

# reset the default colors of the problem cells
# on the 1st digit change after the error display
#	
sub reset_colors {
	my @errcells = grep {$_->is_errcell} @cells;
    return unless @errcells;

	my $field = first {$_->{Button}->cget('-bg') ne 'red'} @cells;
	my $bg_standard = $field->{Button}->cget('-bg');

	foreach my $cell (@errcells) {
			$cell->{Button}->configure(-fg => 'black', -bg => $bg_standard);
		    $cell->is_errcell(0);
	}
	# clear error text in status
	$status_lb->configure( -text => '', -fg => 'black');
    return;
}

# Show message in messageBox widget
#   showmessage(message_lines);
#
sub showmessage {
    $mw->messageBox(@_);
    return;
}

}   # end GUI block

# ====================================================================

# purify the supplied game
#   remove comment lines, newlines
#   ignore whitespace unless used as placeholder
#   my $game_as_string = purify($game_org);
#     $game_org: string or ref to array
#
sub purify {
	my $game = shift;

	if (!ref $game) {
		if ($game =~ m'^#') {
			# needs ignore preceeding comment lines
			my @game = split (qr"\n", $game);
			$game = \@game;
		}
	}

	if (ref $game) {
		if (ref $game eq 'ARRAY'){
			# ignore preceeding comment lines
			while ( ${$game}[0] =~ /^#/ ) { shift @$game };
			$game = join ('', @$game);
		}
		else {
			die 'Parameter "game" must be a string or an array reference';
#			return;
		}
	}

	# ignore whitespace
	if ( length($game) > 81 ) { $game =~ s/\s//g };

	return $game if ( length($game) == 81 );

	my $l = length($game);
    $game =~ s/(.{9})(?=.)/$1\n/g;
	my @msg = ("Length of puzzle string is $l, should be 81\n\n$game\n");
	die @msg;
}

# ====================================================================
# puzzle verification stuff
# ====================================================================

# verify the puzzle that was passed to a start method
# the puzzle is purified already
#   $err_ref = verify_game($gamestring);
#     $err_ref is a ref to the first error info as returned 
#     by the error check routines
# NOTE: The cell objects will be populated with the puzzle
#
sub verify_game {
    my $gamestring = shift;
	return unless $gamestring;

		my @game = split(//, $gamestring);
		foreach my $cell_idx (0 .. 80) {
            my $cell = $cells[$cell_idx];
			$cell->{Value} = ($game[$cell_idx] =~ /[1-9]/ ? $game[$cell_idx] : '');
		}
	return has_doubles() || cell_nocand() || unit_nocand();
}

# verify the state of the sudoku board
# callback of the 'Done' button
#   $err_ref = verify_board();
#     $err_ref is a ref to the first error info as returned 
#     by the error check routines
#     An error will inhibit the end of the module
#
sub verify_board {
#    my $class    = shift;

	my $err_ref = has_doubles() || cell_nocand() || unit_nocand();
	if ($err_ref) {
		mark_problem_cells($err_ref);
	}
	return $err_ref;
}

# for each value cell search for a sibling with the same value
#
sub has_doubles {
	my @presets = grep {$_->{Value}} @cells;
	foreach my $idx1 (0 .. $#presets-1) {
		my $val1 = $presets[$idx1]->{Value};

		my @dupl;
		foreach my $sibltype (qw/Block_num Row_num Col_num/) {
			my $unitname = $presets[$idx1]->{$sibltype};
			push @dupl, grep {
					$_->{$sibltype} eq $unitname
					and $_->{Value} eq $val1
				  } @presets[$idx1+1 .. $#presets];
		}
		next unless @dupl;

		unshift @dupl, $presets[$idx1];
		return ["duplicate value $val1", \@dupl];
	}
	return;
}

# for each empty cell check whether each poss. value is occupied by siblings
#
sub cell_nocand {
	my @presets = grep {$_->{Value}} @cells;

	foreach my $cell (@cells) {
		next if $cell->{Value};
		my @sibls;
		foreach my $sibltype (qw/Block_num Row_num Col_num/) {
			my $typeidx = $cell->{$sibltype};
			push @sibls, grep {$_->{$sibltype} eq $typeidx}  @presets;
		}
		my %seen;
		foreach (@sibls) {$seen{$_->{Value}}++;};
		next if keys %seen != 9;

		return ["no value possible", [$cell]];
	}
	return;
}


# for each unit check whether any poss. value is invalid for each member cell
#
sub unit_nocand {
	my @presets = grep {$_->{Value}} @cells;

UNIT:
	foreach my $type (qw/Block_num Row_num Col_num/) {
		foreach my $unitidx (1 .. 9) {
			# collect the cells of this unit
			my $unittype = substr($type, 0, 1);
			my $unitname = lc ($unittype) . $unitidx;
			my @unitcells = grep {$_->{$type} eq $unitname  
                                  and  not $_->{Value}
                                 } @cells;
            # skip if all values found in this unit
            next UNIT unless @unitcells;   

			my $cands;
			foreach my $cell (@unitcells) {
				my %seen;
				$seen{$_} = undef foreach (1 .. 9);   # define all keys 1..9
				# collect the siblings of this cell
				my @sibls = $cell->_sibling_cells(\@presets);

				# all values of siblings are invalid as cands in this cell

				foreach my $cand (1 .. 9) {
					foreach my $sibl (@sibls) {
						my $val = $sibl->{Value};
						delete $seen{$val} if exists $seen{$val};
					}
				}
				# collect the valid cands
				$cands .= join '|', (sort keys %seen);
			}
			
			foreach my $val (1 .. 9) {
				next if $cands =~ /$val/;
				# ... and values in this unit are invalid too in the cell,
				# but not in the unit
				next if grep {$_->{$type} eq $unitname  
                              and  $_->{Value}  eq $val
                             } @cells;

				return ["value $val not possible", \@unitcells];
			}
		}
	}
	return;
}

# ====================================================================

# Callback of the "Save & Cancel" Button
#
sub save_sudoku {
	my $mw = shift;

	my $file = ask_filename($mw);
        return unless defined $file;
	my $game = mk_result();
        my $ok =  _write_text($mw, $file, $game);
	return $ok;
}

    sub ask_filename {
        my $mw = shift;
        my $file;

        $file = $mw->getSaveFile(
            -title       => 'Sudoku output file',
            -filetypes => [
				[ 'Sudoku Files', '.sudo' ],
				[ 'Text Files', [ '.txt', '.text' ] ],
				[ 'All Files', ['*'] ],
			],
            -defaultextension => '.sudo',
        );
        return unless defined $file;
        use Encode;
        $file = encode( 'iso-8859-1', $file );
		return $file;
    }

# write text to a file
#	_write_text($mw, $outfile, text);
#
sub _write_text {
    my ($mw, $outfile, $text ) = @_;

    open( my $out, '>', $outfile )  or  do {
	  fatal_err($mw, "Cannot open $outfile:\n$!");
	  return;
	};
	print $out $text;
    close($out) or do {
	  fatal_err($mw, "Cannot close $outfile:\n$!");
	  return;
	};
	return 1;
}

sub fatal_err { 
    showmessage(
        -title   => 'Fatal error',
        -message => "@_",
        -icon    => 'error'
    );
    return;
}

# ====================================================================
#package Sudoku::Trainer::Cell;
package Cell;
# ====================================================================

# constructor for cell objects
#
sub new {
    my $class    = shift;
    my ($cell_idx, $button) = @_;    # cell index (0 .. 80), Button widget

    my $row   = int( $cell_idx / 9 ) + 1;
    my $col   = $cell_idx % 9 + 1;
    my $block = int( ( $col - 1 ) / 3 ) + 3 * int( ( $row - 1 ) / 3 ) + 1;

    # cell properties
	
    my %props = (    # cell properties
#        'Name',       "r${row}c$col",    # for tests
        'Cell_num' =>  $cell_idx,
        'Row_num' =>   "r$row",          # row name (r0 .. r8)
        'Col_num' =>   "c$col",          # col name (c0 .. c8)
        'Block_num' => "b$block",        # blk name (b0 .. b8)
        'Value' =>     '',                # cell value
		'Button' => $button,
    );
	
	my $self = \%props;
	bless ($self, $class);
	return $self;
}

# class properties

my %Class;
$Class{unknown_digit} = '-';   # default

# setter/getter for property 'unknown_digit'
#
sub placeholder {
    my ($class, $char ) = @_;

	if ($char) {
		$Class{unknown_digit} = $char;
		return;
	}
	return $Class{unknown_digit};
}

# setter/getter for property 'act_field'
# the active field is the Button that belongs to the current cell
#
sub activefield {
    my ($class, $cell ) = @_;

	if ($cell) {
		$Class{act_field} = $cell;
		return;
	}
	return $Class{act_field} || 0;
}

# setter/getter for property 'err'
#
sub is_errcell {
    my ($self, $bool ) = @_;

	if ($bool) {
		$self->{err} = $bool;
		return;
	}
	return $self->{err};
}

#  return all siblings of a given cell
#    my @sibling_cells = $cell->_sibling_cells($presets_ref);
#      $presets_ref: Ref to array with preset values
#
sub _sibling_cells {
    my ($self, $presets_ref) = @_;

	my @sibls;
		foreach my $sibltype (qw/Block_num Row_num Col_num/) {
			my $unitname = $self->{$sibltype};
			push @sibls, grep {
					$_->{$sibltype} eq $unitname
				  } @$presets_ref;
		}
	return @sibls;
}

1;

__END__

=head1 NAME

B<Games::Sudoku::Preset> - enter, edit or validate the preset values of a Sudoku puzzle.

=head1 VERSION 

This documentation refers to B<Games::Sudoku::Preset> version 0.0.1.

=head1 SYNOPSIS

    use Games::Sudoku::Preset;

    # Enter the preset values for a new Sudoku puzzle
    my $puzzle = Games::Sudoku::Preset->enter();

    # Edit an existing Sudoku puzzle
    my $puzzle = Games::Sudoku::Preset->edit($game);

    # Validate an existing Sudoku puzzle
    my $puzzle = Games::Sudoku::Preset->validate($game);

I<$game> is either a string (possibly with embedded newlines) or a reference
to an array of strings.

I<$puzzle> is a string of 81 characters, representing a validated Sudoku puzzle.

=head1 DESCRIPTION 

This section describes the common behaviour of the three public start methods.
Special behaviour of a specific start method is described in section 
L</METHODS>.

In general module Games::Sudoku::Preset works in 3 steps:

=over 4

=item * Purify (convert to a string of 81 characters)

=item * Verify (check for errors, display errors on the board)

=item * Edit (let the user edit the puzzle)

=item * Return (Verify the edited puzzle, return if no errors found)

=back

=head2 Purify the given puzzle

When the puzzle is passed as a reference to an array, this is joined to a string.
The number and positions of newlines in the string doesn't matter: all newlines
are removed. If the string is now longer than 81 characters, all whitespace
is removed. Now the string must be exactly 81 characters long. 
Otherwise the string is written to STDERR together with an error message,
and an empty string is returned to the caller.

=head2 Verify the specified puzzle

The puzzle is checked for violation of the well known Sudoku rules (e. g.
twice the same value in a row). When an error is found, a message is
displayed below the Sudoku board of the GUI, and the affected fields are marked
by red color on the board. The user may now correct the error. Editing of the
board is described in detail in section L</EDIT THE SUDOKU BOARD>.

=head2 The editing phase

The user edits the puzzle in the GUI board according to his needs. This 
is described in detail in section L</EDIT THE SUDOKU BOARD>.

=head2 Return of the edited puzzle

The user clicks on the I<Done> button to leave Games::Sudoku::Preset. First the 
current state of the puzzle is verified again to ensure that it is ok. When an
error is found, Games::Sudoku::Preset shows this on the board and stays in the 
editing phase.
Otherwise the current puzzle is returned, using the first placeholder of
the original puzzle as the placeholder for unknown values.

=head1 METHODS

This section describes specific behaviour of the public start methods.

=head2 B<enter>

Method Games::Sudoku::Preset->enter initially displays an empty Sudoku board.
The user may immediately start entering values, as descibed in section
L</EDIT THE SUDOKU BOARD>.

The returned puzzle has "-" as the placeholder for unknown values.

=head2 B<edit>

Method Games::Sudoku::Preset->edit displays the initially verified puzzle 
on the Sudoku board, whether
with or without errors. The user may immediately start editing it,
as descibed in section L</EDIT THE SUDOKU BOARD>.

=head2 B<validate>

Method Games::Sudoku::Preset->validate returns the initially verified puzzle 
immediately when no errors are found. The GUI is not shown in this case.


=head1 EDIT THE SUDOKU BOARD

This section describes the usage of the Sudoku board. Editing may be 
done via the mouse or via the keyboard.

=head2 Editing via the mouse

When the mouse cursor enters a field of the board, this field gets covered
by a 3x3 grid of tiny squares. Each square corresponds to one of the digits 
1 to 9. If the field already contains a value, the corresponding square is
shown in red. 

Clicking on one of the black squares inserts the corresponding
digit as the value of the field. So you can select a field and a digit with
a single mouse click (I am very proud on this invention). Any previous
value of the field will be replaced. 
Clicking on the red square will remove the corresponding 
value from the field.

=head2 Editing via the keyboard 

The input focus may be moved to an adjacent field by the I<arrow> keys (end
around). A value may be entered in the active field by the keys I<1> to I<9> (on
the alpha or numeric keypad). This will replace any previous value in the field.
A value may be deleted by the keys I<0>, I<Delete>, or the I<Space bar>.

Pressing any of the supported keys will also hide the 3x3 grid.

=head1 TERMINATING THE MODULE 

The GUI of B<Games::Sudoku::Preset> shows three buttons for termination. 

The I<Done> button causes validation of the current state of the puzzle. When 
no errors are found, it is returned to the caller.

The I<Cancel> button causes termination of the program. No output is generated.

The I<Save & Cancel> button lets you save the current state of the puzzle in a
file before terminating the program. You may later continue to edit it.

The standard kill button (at the right side of the title bar) acts like the 
Cancel button.

=head1 TRANSFORMATIONS OF THE RETURNED PUZZLE

The puzzle is returned as a string of 81 characters. According to the needs of
the program, these transformations may be used:

=over 4

=item * Change the placeholder

    $puzzle =~ tr/-/<my_placeholder>/;

=item * Change to string of 9 lines with 9 characters each

	(my $puzzle9x9 = $puzzle) =~ s/(.{9})(?=.)/$1\n/g;

=item * Change to array of 9 elements with 9 characters each

	my @puzzle9x9;
	while ($puzzle) {push @puzzle9x9, substr($puzzle, 0, 9, '')};

=back

=head1 RESTRICTIONS

The placeholders in the given Sudoku puzzle must be printable ASCII characters.

For historical reasons "#" shouldn't be used as the first placeholder.

=head1 DEPENDENCIES 

=over 2

=item * L<http://search.cpan.org/perldoc?Tk> (PerlE<sol>Tk)

=back

=head1 BUGS

Please report bugs using <http://rt.cpan.org>. Patches are welcome.

=head1 AUTHOR

Klaus Wittrock  (Wittrock#cpan.org)

=head1 LICENCE AND COPYRIGHT

Copyright 2014 Klaus Wittrock. All Rights Reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

